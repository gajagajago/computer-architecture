import Vector::*;

import FftCommon::*;
import Fifo::*;

interface Fft;
  method Action enq(Vector#(FftPoints, ComplexData) in);
  method ActionValue#(Vector#(FftPoints, ComplexData)) deq;
endinterface

(* synthesize *)
module mkFftCombinational(Fft);
  Fifo#(2, Vector#(FftPoints, ComplexData)) inFifo <- mkCFFifo;
  Fifo#(2, Vector#(FftPoints, ComplexData)) outFifo <- mkCFFifo;
  Vector#(NumStages, Vector#(BflysPerStage, Bfly4)) bfly <- replicateM(replicateM(mkBfly4));

  function Vector#(FftPoints, ComplexData) stage_f(StageIdx stage, Vector#(FftPoints, ComplexData) stage_in);
    Vector#(FftPoints, ComplexData) stage_temp, stage_out;
    for (FftIdx i = 0; i < fromInteger(valueOf(BflysPerStage)); i = i + 1)
    begin
      FftIdx idx = i * 4;
      Vector#(4, ComplexData) x;
      Vector#(4, ComplexData) twid;
      for (FftIdx j = 0; j < 4; j = j + 1 )
      begin
        x[j] = stage_in[idx+j];
        twid[j] = getTwiddle(stage, idx+j);
      end
      let y = bfly[stage][i].bfly4(twid, x);

      for(FftIdx j = 0; j < 4; j = j + 1 )
        stage_temp[idx+j] = y[j];
    end

    stage_out = permute(stage_temp);

    return stage_out;
  endfunction

  rule doFft;
    inFifo.deq;
    Vector#(4, Vector#(FftPoints, ComplexData)) stage_data;
    stage_data[0] = inFifo.first;

    for (StageIdx stage = 0; stage < 3; stage = stage + 1)
      stage_data[stage+1] = stage_f(stage, stage_data[stage]);
    outFifo.enq(stage_data[3]);
  endrule

  method Action enq(Vector#(FftPoints, ComplexData) in);
    inFifo.enq(in);
  endmethod

  method ActionValue#(Vector#(FftPoints, ComplexData)) deq;
    outFifo.deq;
    return outFifo.first;
  endmethod
endmodule

(* synthesize *)
module mkFftFolded(Fft);
  Fifo#(2, Vector#(FftPoints, ComplexData)) inFifo <- mkCFFifo;
  Fifo#(2, Vector#(FftPoints, ComplexData)) outFifo <- mkCFFifo;
  Vector#(BflysPerStage, Bfly4) bfly <- replicateM(mkBfly4);
  Reg#(Vector#(FftPoints, ComplexData)) sReg <- mkRegU;
  Reg#(StageIdx) stageIdx <- mkReg(0);

  function Vector#(FftPoints, ComplexData) stage_f(StageIdx stage, Vector#(FftPoints, ComplexData) stage_in);
    Vector#(FftPoints, ComplexData) stage_temp, stage_out;
    for (FftIdx i = 0; i < fromInteger(valueOf(BflysPerStage)); i = i + 1)
    begin
      FftIdx idx = i * 4;
      Vector#(4, ComplexData) x;
      Vector#(4, ComplexData) twid;
      for (FftIdx j = 0; j < 4; j = j + 1 )
      begin
        x[j] = stage_in[idx+j];
        twid[j] = getTwiddle(stage, idx+j);
      end
      let y = bfly[i].bfly4(twid, x);

      for(FftIdx j = 0; j < 4; j = j + 1 )
        stage_temp[idx+j] = y[j];
    end

    stage_out = permute(stage_temp);

    return stage_out;
  endfunction

  rule foldedEntry (stageIdx == 0);
    	  sReg <= stage_f(stageIdx, inFifo.first());
	  stageIdx <= stageIdx + 1;
	  inFifo.deq();
  endrule

  rule foldedCirculate ((stageIdx != 0) && (stageIdx < 2));
  	sReg <= stage_f(stageIdx, sReg);
	stageIdx <= stageIdx + 1;
  endrule

  rule foldedExit (stageIdx == 2);
  	outFifo.enq(stage_f(stageIdx, sReg)); 
	stageIdx <= 0;
  endrule


  method Action enq(Vector#(FftPoints, ComplexData) in);
    inFifo.enq(in);
  endmethod

  method ActionValue#(Vector#(FftPoints, ComplexData)) deq;
    outFifo.deq;
    return outFifo.first;
  endmethod
endmodule

(* synthesize *)
module mkFftPipelined(Fft);
  Fifo#(2, Vector#(FftPoints, ComplexData)) inFifo <- mkCFFifo;
  Fifo#(2, Vector#(FftPoints, ComplexData)) outFifo <- mkCFFifo;
  Fifo#(2, Vector#(FftPoints, ComplexData)) fifo1 <- mkCFFifo;
  Fifo#(2, Vector#(FftPoints, ComplexData)) fifo2 <- mkCFFifo;
  Vector#(NumStages, Vector#(BflysPerStage, Bfly4)) bfly <- replicateM(replicateM(mkBfly4));

  // You can copy & modify the stage_f function in the combinational implementation.
  function Vector#(FftPoints, ComplexData) stage_f(StageIdx stage, Vector#(FftPoints, ComplexData) stage_in);
    Vector#(FftPoints, ComplexData) stage_temp, stage_out;
    for (FftIdx i = 0; i < fromInteger(valueOf(BflysPerStage)); i = i + 1)
    begin
      FftIdx idx = i * 4;
      Vector#(4, ComplexData) x;
      Vector#(4, ComplexData) twid;
      for (FftIdx j = 0; j < 4; j = j + 1 )
      begin
        x[j] = stage_in[idx+j];
        twid[j] = getTwiddle(stage, idx+j);
      end
      let y = bfly[stage][i].bfly4(twid, x);

      for(FftIdx j = 0; j < 4; j = j + 1 )
        stage_temp[idx+j] = y[j];
    end

    stage_out = permute(stage_temp);

    return stage_out;
  endfunction


  rule stage0;
	  let x = inFifo.first(); inFifo.deq();
	  fifo1.enq(stage_f(0, x));
  endrule

  rule stage1;
          let x = fifo1.first(); fifo1.deq();
          fifo2.enq(stage_f(1, x));
  endrule

  rule stage2;
	  let x = fifo2.first(); fifo2.deq();
	  outFifo.enq(stage_f(2, x));
  endrule


  method Action enq(Vector#(FftPoints, ComplexData) in);
    inFifo.enq(in);
  endmethod

  method ActionValue#(Vector#(FftPoints, ComplexData)) deq;
    outFifo.deq;
    return outFifo.first;
  endmethod
endmodule

interface SuperFoldedFft#(numeric type radix);
  method ActionValue#(Vector#(FftPoints, ComplexData)) deq;
  method Action enq(Vector#(FftPoints, ComplexData) in);
endinterface

module mkFftSuperFolded(SuperFoldedFft#(radix)) provisos(Div#(TDiv#(FftPoints, 4), radix, times), Mul#(radix, times, TDiv#(FftPoints, 4)));
  Fifo#(2, Vector#(FftPoints, ComplexData)) inFifo <- mkCFFifo;
  Fifo#(2, Vector#(FftPoints, ComplexData)) outFifo <- mkCFFifo;
  
  Vector#(radix, Bfly4) bfly <- replicateM(mkBfly4);
  Reg#(Vector#(64, ComplexData)) rg_in <- mkRegU;
  Vector#(16, Reg#(Vector#(4, ComplexData))) rg_outs <- replicateM(mkRegU); 
  Reg#(FftIdx) step <- mkReg(0);
  Reg#(StageIdx) stage <- mkReg(0);
//  Reg#(Vector#(64, ComplexData)) stage_out <- mkRegU;


/*
  function Vector#(64, ComplexData) two_bfly4(StageIdx step);

	  Vector#(64, ComplexData) stage_temp, stage_out;
	  
	  for (Integer i = 0; i < 8; i = i + 1) begin
		Integer idx = i * 8;
		Vector#(4, ComplexData) x1, x2;

		x1[0] = rg_in[step-1][idx]; x1[1] = rg_in[step-1][idx+1]; x1[2] = rg_in[step-1][idx+2]; x1[3] = rg_in[step-1][idx+3];
                x2[0] = rg_in[step-1][idx+4]; x2[1] = rg_in[step-1][idx+5]; x2[2] = rg_in[step-1][idx+6]; x2[3] = rg_in[step-1][idx+7];

		Vector#(4, ComplexData) twid;
		twid[0] = getTwiddle(step, 0);twid[1] = getTwiddle(step, 1);twid[2] = getTwiddle(step, 2);twid[3] = getTwiddle(step, 3);

		let y1 = bfly[0].bfly4(twid, x1);
		let y2 = bfly[1].bfly4(twid, x2);

		stage_temp[idx] = y1[0];stage_temp[idx+1] = y1[1];stage_temp[idx+2] = y1[2];stage_temp[idx+3] = y1[3];
		stage_temp[idx+4] = y2[0];stage_temp[idx+5] = y2[1];stage_temp[idx+6] = y2[2];stage_temp[idx+7] = y2[3];
	end

	stage_out = permute(stage_temp);

	return stage_out;
  endfunction
*/
		/*
  rule fft_start(step == 0);
	  $display("START FFT");
	  //step <= step + 1;
	  //stage <= stage + 1;
	  rg_in <= inFifo.first(); inFifo.deq();
  endrule
*/

  rule fft_start(stage == 0 && step == 0);
  	  $display("Start!");
	  rg_in <= inFifo.first(); inFifo.deq();
	  stage <= stage + 1;
  endrule

  rule step_Bfly4(stage != 0 && step < 8);
	  $display("BFLY step %d", step);
// 	  rg_in <= inFifo.first(); inFifo.deq();
	  Vector#(4, ComplexData) x1, x2;
	  let idx = step * 8;
          x1[0] = rg_in[idx]; x1[1] = rg_in[idx+1]; x1[2] = rg_in[idx+2]; x1[3] = rg_in[idx+3];
          x2[0] = rg_in[idx+4]; x2[1] = rg_in[idx+5]; x2[2] = rg_in[idx+6]; x2[3] = rg_in[idx+7];

	  
          Vector#(4, ComplexData) twid;
          twid[0] = getTwiddle(stage, idx);twid[1] = getTwiddle(stage, idx+1);twid[2] = getTwiddle(stage, idx+2);twid[3] = getTwiddle(stage, idx+3);
          Vector#(4, ComplexData) twid2;
          twid2[0] = getTwiddle(stage, idx+4);twid2[1] = getTwiddle(stage, idx+5);twid2[2] = getTwiddle(stage, idx+6);twid2[3] = getTwiddle(stage, idx+7);
	

	 // Vector#(4, ComplexData) twid;
          let y1 = bfly[0].bfly4(twid, x1);
          let y2 = bfly[1].bfly4(twid2, x2);
 	  rg_outs[step*2] <= y1; rg_outs[step*2+1] <= y2;
	  //$display(rg_outs);
	  //stage_out[idx] <= y1[0];stage_out[idx+1] <= y1[1];stage_out[idx+2] <= y1[2];stage_out[idx+3] <= y1[3];
	//  rg_outs[idx] <= y1[0];rg_outs[idx+1] <= y1[1];rg_outs[idx+2] <= y1[2];rg_outs[idx+3] <= y1[3];
	 // rg_outs[idx+4] <= y2[0];rg_outs[idx+5] <= y2[1];rg_outs[idx+6] <= y2[2];rg_outs[idx+7] <= y2[3];
	  step <= step + 1;
  endrule

  rule end_steps(stage < 3 && step == 8);
  $display("Reached end of steps");
	  step <= 0;
	  stage <= stage + 1;
	  //stage <= stage + 1;
	  Vector#(64, ComplexData) temp;
	  for (Integer i = 0; i < 16; i = i+1) begin
		  for (Integer j = 0; j < 4; j = j+1) begin
			  temp[i*4+j] = rg_outs[i][j];
			  $display("idx %d: %x", i*4+j, rg_outs[i][j]);
		  end
	  end

	  rg_in <= permute(temp);
  endrule

  rule end_stage(stage == 3 && step == 8);
	  Vector#(64, ComplexData) temp;
          for (Integer i = 0; i < 16; i = i+1) begin
                  for (Integer j = 0; j < 4; j = j+1) begin
                          temp[i*4+j] = rg_outs[i][j];
			  $display("idx %d: %x", i*4+j, rg_outs[i][j]);
		  end
          end

          outFifo.enq(permute(temp));
	  stage <= 0; step <= 0;
	  $display("END OF STAGE");
  endrule


  /*
  rule fft_stage_1 (stage == 1);
	  rg_in[stage] <= two_bfly4(stage);
	  stage <= stage + 1;
  endrule

  rule fft_stage_2 (stage == 2);
          rg_in[stage] <= two_bfly4(stage);
          stage <= stage + 1;
  endrule

  rule fft_stage_3 (stage == 3);
          rg_in[stage] <= two_bfly4(stage);
          stage <= stage + 1;
  endrule
  rule fft_end (step == 8);
	  outFifo.enq(rg_in);
	  step <= 0;
  endrule
*/
  method Action enq(Vector#(FftPoints, ComplexData) in);
    inFifo.enq(in);
  endmethod

  method ActionValue#(Vector#(FftPoints, ComplexData)) deq;
    outFifo.deq;
    return outFifo.first;
  endmethod
endmodule

function Fft getFft(SuperFoldedFft#(radix) f);
  return (interface Fft;
    method enq = f.enq;
    method deq = f.deq;
  endinterface);
endfunction
