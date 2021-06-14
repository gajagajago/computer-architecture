import Types::*;
import CMemTypes::*;
import CacheTypes::*;
import Fifo::*;
import RegFile::*;
import Vector::*;
import MemInit::*; 

interface Cache;
	method Action req(MemReq r);
	method ActionValue#(Data) resp;

 	method ActionValue#(CacheMemReq) memReq;
 	method Action memResp(Line r);

	method Data getMissCnt;
	method Data getTotalReq;
endinterface

typedef enum {Ready, StartMiss, SendFillReq, WaitFillResp} CacheStatus deriving (Bits, Eq);


(*synthesize*)
module mkCacheSetAssociative (Cache);
	Vector#(LinesPerSet, RegFile#(CacheIndex, Line))				  dataArray <- replicateM(mkRegFileFull);
	Vector#(LinesPerSet, RegFile#(CacheIndex, Maybe#(CacheTag)))       tagArray <- replicateM(mkRegFileFull);
	Vector#(LinesPerSet, RegFile#(CacheIndex, Bool))				 dirtyArray <- replicateM(mkRegFileFull);
	Vector#(LinesPerSet, RegFile#(CacheIndex, SetOffset)) lruArray <- replicateM(mkRegFileFull);

	Reg#(Bit#(TAdd#(SizeOf#(CacheIndex), 1))) init <- mkReg(0);
	Reg#(CacheStatus)					    status <- mkReg(Ready);
	Reg#(CacheStatus)					    testflag <- mkReg(Ready);
	Reg#(Maybe#(SetOffset)) 		targetLine <- mkReg(Invalid);

	Fifo#(1, Data)  hitQ <- mkBypassFifo;
	Reg#(MemReq) missReq <- mkRegU;

	Fifo#(2, CacheMemReq) memReqQ <- mkCFFifo;
	Fifo#(2, Line) 		 memRespQ <- mkCFFifo;

	Reg#(Data) missCnt <- mkReg(0);
	Reg#(Data)  reqCnt <- mkReg(0);

	function CacheIndex getIdx(Addr addr) = truncate(addr >> (2 + fromInteger(valueOf(SizeOf#(BlockOffset))))); 
	function CacheTag getTag(Addr addr) = truncateLSB(addr);
	function BlockOffset getOffset(Addr addr) = truncate(addr >> 2); 

	function Addr getBlockAddr(CacheTag tag, CacheIndex idx);
		BlockOffset def_offset = 0;
		Addr addr = {tag, idx, def_offset, 2'b0}; 
		return addr;
	endfunction

	function Maybe#(SetOffset) checkHit(CacheTag tag, CacheIndex idx);
		// Returns the SetOffset when cache hit occurs at given idx with the given tag.
		// It happens by checking the validity and tag value.
		Maybe#(SetOffset) ret = Invalid;

		for(Integer i = 0; i < valueOf(LinesPerSet); i = i + 1)
		begin
			let tagArrayVal = tagArray[i].sub(idx);

			if(isValid(tagArrayVal) && (fromMaybe(?, tagArrayVal) == tag) )
			begin
				ret = tagged Valid fromInteger(i);
			end
		end
		return ret;
	endfunction

	function Maybe#(SetOffset) findInvalid(CacheIndex idx);
		// Returns the SetOffset of a invalid cache slot at given idx.
		// If no one exists, returns Invalid.
		Maybe#(SetOffset) ret = Invalid;

		for(Integer i = 0; i < valueOf(LinesPerSet); i = i+1)
		begin
			if(!isValid(tagArray[i].sub(idx)))
			begin
				ret = tagged Valid fromInteger(i);
			end
		end

		return ret;
	endfunction

    function SetOffset findLRU(CacheIndex idx);
		// Returns the exact LRU.
		return lruArray[valueOf(LinesPerSet) - 1].sub(idx);
	endfunction

	function Action updateLRUArray(CacheIndex idx, SetOffset lineNum);
		// update lruArray to help finding LRU.
	    return action
			// find the index of lineNum element in the LRUArray.
	       	Integer idxInLRUArray = 0;
        	for (Integer i = 1; i < valueOf(LinesPerSet); i = i+1)
        	begin
            	if (lineNum == lruArray[i].sub(idx)) begin
            		idxInLRUArray = i;
          		end
			end

			// right shift elements before lineNum.
          	for (Integer i = 1;  i<= idxInLRUArray; i = i+1)
          	begin
            	lruArray[i].upd(idx, lruArray[i-1].sub(idx));
          	end

          	// put lineNum at the front.
          	lruArray[0].upd(idx, lineNum);
	    endaction;
    endfunction

	

	/* You can use this function in rules(startMiss,waitFillResp) when implement set associative cache */
	function SetOffset findLineToUse(CacheIndex idx);
		// if empty line exists, use that line.
		// if empty line doesn't exist, use LRU.
		let emptyLine = findInvalid(idx);
		if (isValid(emptyLine)) begin
			return fromMaybe(?, emptyLine);
		end else begin
			return findLRU(idx);
		end
	endfunction



 	let inited = truncateLSB(init) == 1'b1;

	rule initialize(!inited);
		init <= init + 1;
		$display("INITIALIZING CACHE");
		for(Integer i = 0; i< valueOf(LinesPerSet);i = i+1)
		begin
			tagArray[i].upd(truncate(init), Invalid);
			dirtyArray[i].upd(truncate(init), False);
			lruArray[i].upd(truncate(init), fromInteger(i));
		end
	endrule

	rule startMiss(status == StartMiss);
		$display("\n");
                $display("Entered start miss stage");

                /* TODO: Implement here */
                // Load + Miss : Request memory. If dirty, request memory to update dirty address.
                let idx = getIdx(missReq.addr);
                let tag = tagArray[0].sub(idx);  // CHECK
                let dirty = dirtyArray[0].sub(idx);

        	if (isValid(tag) && dirty) begin
			let addr = getBlockAddr(validValue(tag), idx);
                        let data = dataArray[0].sub(idx);
                        memReqQ.enq(CacheMemReq{op:St, addr:addr, data:data, burstLength:fromInteger(valueOf(WordsPerBlock))});
			// dirty tag .. 
			dirtyArray[0].upd(idx, False);

                end

                status <= SendFillReq;

	endrule

	rule sendFillReq(status == SendFillReq);
		 $display("\n");
                $display("Entered send fill req");
		$display("Request to dMem : addr", missReq.addr);
		let idx = getIdx(missReq.addr);
                let tag = getTag(missReq.addr); // Changed frm getTag(missreq.addr) -> tagArray;
		let offset = getOffset(missReq.addr);
		//Addr addr = {validValue(tag), idx, offset, 2'b0};
                //let addr = getBlockAddr(validValue(tag), idx);
		//let addr = missReq.addr;
		Addr addr = {tag, idx, 0, 2'b0};
		$display("r.addr %b", missReq.addr);
		$display("block addr: %b", getBlockAddr(tag, idx));
		$display("addr with offset : %b", addr);
		
		memReqQ.enq(CacheMemReq{op:missReq.op, addr:addr, data:?, burstLength:fromInteger(valueOf(WordsPerBlock))});
                status <= WaitFillResp;
	endrule

	rule waitFillResp(status == WaitFillResp);
		                $display("\n");
                $display("Entered wait fill req");
                let receivedBlock = memRespQ.first;
                let idx = getIdx(missReq.addr);
                let tag = getTag(missReq.addr); // CHECK

		$display("Received block from dMem %b", receivedBlock);
                dataArray[0].upd(idx, receivedBlock);
                tagArray[0].upd(idx, Valid(tag));
                //dirtyArray[0].upd(idx, False);
                let offset = getOffset(missReq.addr); $display("Required block offset: %d", offset);

		$display("[0]: %d", receivedBlock[0]);
                $display("[1]: %d", receivedBlock[1]);
		$display("[2]: %d", receivedBlock[2]);
		$display("[3]: %d", receivedBlock[3]);

		hitQ.enq(receivedBlock[offset]); $display("Returned data to cpu : %d", receivedBlock[offset]);
                memRespQ.deq;
                status <= Ready;
	endrule

	method Action req(MemReq r) if (status == Ready && inited);
		let tag = getTag(r.addr); $display("Tag : %b", tag);
                let idx = getIdx(r.addr); $display("Addr : %b", idx);
		let hit = checkHit(tag, idx);

                if (r.op == Ld) begin
                        $display("Cache -- Load --", r.addr);

                        if (isValid(hit)) begin
                                let blockData = dataArray[0].sub(idx);
				let offset = getOffset(r.addr); $display("Byte offset : %b", offset);
				let data = blockData[offset]; $display("DATA = %b", data);
                                hitQ.enq(data); 
                        end
                        else begin
				$display("Miss. Start request to dMem.");
                                missReq <= r;
                                status <= StartMiss;
                        end
                end
                else if (r.op == St) begin 
                        $display("Cache -- Store -- ", r.addr);

                        if (isValid(hit)) begin
				$display("Hit. DATA = ", r.data);
				let currBlockData = dataArray[0].sub(idx);
				let offset = getOffset(r.addr); $display("byte offset : %b", offset);
//				$display("curr block[0] %b", currBlockData[0]);				                                 $display("curr block[1] %b", currBlockData[1]);
  //                              $display("curr block[2] %b", currBlockData[2]);				                                 $display("curr block[3] %b", currBlockData[3]);

				currBlockData[offset] = r.data;
                                dataArray[0].upd(idx, currBlockData);
                                dirtyArray[0].upd(idx, True);
                        end
                        else begin
				$display("Miss. Write no allocate");
				$display("Total addr: %b", r.addr);
				$display("Tag: %b", tag, "   Idx: %b", idx, "   byte offset: %b", getOffset(r.addr));
                               
				let storeLine = dataArray[0].sub(idx);
				storeLine[0] = r.data;
				$display("data to dmem: %b ", storeLine[0]);

				//let addr = getBlockAddr(tag, idx);
                                memReqQ.enq(CacheMemReq{op:r.op, addr:r.addr, data:storeLine, burstLength:1});
                        end
                end

		/* DO NOT MODIFY BELOW HERE! */
		
		if(!isValid(hit))
		begin
			missCnt <= missCnt + 1;
			$display("missCnt = ", missCnt);
		end
		reqCnt <= reqCnt + 1;  
		$display("reqCnt = ", reqCnt);	
	endmethod


	method ActionValue#(Data) resp;
		hitQ.deq;
		return hitQ.first;
	endmethod

	method ActionValue#(CacheMemReq) memReq;
		memReqQ.deq;
		return memReqQ.first;
	endmethod

	method Action memResp(Line r);
		memRespQ.enq(r);
	endmethod

	method Data getMissCnt;
		return missCnt;
	endmethod

	method Data getTotalReq;
		return reqCnt;
	endmethod
endmodule

(*synthesize*)
module mkCache (Cache);
	Cache cacheSetAssociative <- mkCacheSetAssociative;
	return cacheSetAssociative;
endmodule
