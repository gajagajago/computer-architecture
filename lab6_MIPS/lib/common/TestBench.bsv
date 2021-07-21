import ThreeStageHavard::*;

(* synthesize *)
module mkTB();
    Proc proc <- mkProc;
    
    Reg#(Bit#(32)) cyc <- mkReg(0);
    Reg#(Bit#(2))  stg <- mkReg(0);

    rule setpc(stg==0);
        proc.hostToCpu(32'h1000);
        stg <= stg + 1;
    endrule

    rule getstatus(stg==1);
        cyc <= cyc + 1;
        $display("\ncycle %d", cyc);
        
        if(cyc==150000) begin
            $fdisplay(stderr, "It's been a long time running...");
            $finish;
        end

        let c = proc.cpuToHost(0);
        if(c!=0) begin
		    $fwrite(stderr, "\n");
	        if(c==1) $fwrite(stderr, "PASSED");
	        else $fwrite(stderr, "FAILED %d", c);
	        stg <= stg + 1;
	        $fwrite(stderr, "\n");
        end
    endrule

    rule getinst(stg==2);
        $display("\ncycle %d", cyc);
        $fwrite(stderr, "inst   %d\n", proc.cpuToHost(2));
        stg <= stg + 1;
    endrule

    rule getcyc(stg==3);
        $fwrite(stderr, "cyc    %d\n", proc.cpuToHost(3));
        $finish;
    endrule
endmodule

