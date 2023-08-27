module clk (
   input int G_HYPERRAM_FREQ_MHZ,
   input real G_HYPERRAM_PHASE,
   output logic clk_x1_o,
   output logic clk_x2_o,
   output logic clk_x2_del_o,
   output logic rst_o
);
    
   time C_CLK_PERIOD = (1000/G_HYPERRAM_FREQ_MHZ) * 1 ns;

   initial begin
      clk_x1_o = 1'b1;
      forever begin
         #C_CLK_PERIOD/2;
         clk_x1_o = 1'b0;
         #C_CLK_PERIOD/2;
         clk_x1_o = 1'b1;
      end
   end

   initial begin
      clk_x2_o = 1'b1;
      forever begin
         #C_CLK_PERIOD/4;
         clk_x2_o = 1'b0;
         #C_CLK_PERIOD/4;
         clk_x2_o = 1'b1;
      end
   end

   initial begin
      #C_CLK_PERIOD/2*(G_HYPERRAM_PHASE/360.0);
      repeat(1) begin
         clk_x2_del_o = 1'b1;
         #C_CLK_PERIOD/4;
         clk_x2_del_o = 1'b0;
         #C_CLK_PERIOD/4;
      end
   end

   initial begin
      rst_o = 1'b1;
      #10*C_CLK_PERIOD;
      repeat(1) begin
         wait(clk_x1_o == 1'b1);
      end
      rst_o = 1'b0;
      wait;
   end

endmodule
