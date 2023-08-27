module hyperram_tb #(parameter string RUNNER_CFG = "");

   // Declare avalon master simulation module, to be used as traffic generator
   //   constant AVALON_BUS : bus_master_t := new_bus(data_length => 16, address_length => 32);

   // Constants
   parameter integer C_HYPERRAM_FREQ_MHZ = 100;
   parameter real C_HYPERRAM_PHASE = 162.000;
   parameter real C_DELAY = 1.0; // Value in nanoseconds
   parameter integer G_DATA_SIZE = 16;

   wire clk_x1;
   wire clk_x2;
   wire clk_x2_del;
   wire rst;

   wire tb_start;

   wire sys_resetn;
   wire sys_csn;
   wire sys_ck;
   wire sys_rwds;
   wire [7:0] sys_dq;
   wire sys_rwds_in;
   wire [7:0] sys_dq_in;
   wire sys_rwds_out;
   wire [7:0] sys_dq_out;
   wire sys_rwds_oe;
   wire sys_dq_oe;

   // HyperRAM simulation device interface
   reg hr_resetn;
   reg hr_csn;
   reg hr_ck;
   wire hr_rwds;
   reg [7:0] hr_dq;

   // Avalon Memory Map interface to HyperRAM Controller
   reg avm_write;
   reg avm_read;
   reg [31:0] avm_address = 32'h0;
   reg [G_DATA_SIZE-1:0] avm_writedata;
   reg [G_DATA_SIZE/8-1:0] avm_byteenable;
   reg [7:0] avm_burstcount;
   reg [G_DATA_SIZE-1:0] avm_readdata;
   reg avm_readdatavalid;
   reg avm_waitrequest;

   // HyperRAM tri-state control signals
   wire hr_rwds_in;
   reg [7:0] hr_dq_in;
   wire hr_rwds_out;
   reg [7:0] hr_dq_out;
   wire hr_rwds_oe;
   reg hr_dq_oe;

   wire [31:0] avm_rnd_addr;
   wire [G_DATA_SIZE-1:0] avm_rnd_data;

   // Avalon master, generate transactions for hyperram controller
   vunit_lib.avalon_master #(
       .BUS_HANDLE(AVALON_BUS)
   ) avalon_mm_master_inst (
       .clk(clk_x1),
       .address(avm_address),
       .byteenable(avm_byteenable),
       .burstcount(avm_burstcount),
       .waitrequest(avm_waitrequest),
       .write(avm_write),
       .writedata(avm_writedata),
       .read(avm_read),
       .readdata(avm_readdata),
       .readdatavalid(avm_readdatavalid)
   );

   // Generate clock and reset
   work_clk i_clk (
       .G_HYPERRAM_FREQ_MHZ(C_HYPERRAM_FREQ_MHZ),
       .G_HYPERRAM_PHASE(C_HYPERRAM_PHASE),
       .clk_x1_o(clk_x1),
       .clk_x2_o(clk_x2),
       .clk_x2_del_o(clk_x2_del),
       .rst_o(rst)
   );

   // Instantiate HyperRAM interface
   src_lib_hyperram i_hyperram (
      .clk_x1_i(clk_x1),
      .clk_x2_i(clk_x2),
      .clk_x2_del_i(clk_x2_del),
      .rst_i(rst),
      .avm_write_i(avm_write),
      .avm_read_i(avm_read),
      .avm_address_i(avm_address),
      .avm_writedata_i(avm_writedata),
      .avm_byteenable_i(avm_byteenable),
      .avm_burstcount_i(avm_burstcount),
      .avm_readdata_o(avm_readdata),
      .avm_readdatavalid_o(avm_readdatavalid),
      .avm_waitrequest_o(avm_waitrequest),
      .hr_resetn_o(sys_resetn),
      .hr_csn_o(sys_csn),
      .hr_ck_o(sys_ck),
      .hr_rwds_in_i(sys_rwds_in),
      .hr_dq_in_i(sys_dq_in),
      .hr_rwds_out_o(sys_rwds_out),
      .hr_dq_out_o(sys_dq_out),
      .hr_rwds_oe_o(sys_rwds_oe),
      .hr_dq_oe_o(sys_dq_oe)
   );

   // Tri-state buffers for HyperRAM
   assign sys_rwds = sys_rwds_oe ? sys_rwds_out : 1'bz;
   assign sys_dq = sys_dq_oe ? sys_dq_out : 8'bz;
   assign sys_rwds_in = sys_rwds;
   assign sys_dq_in = sys_dq;

   // Connect controller to device (with delay)
   assign hr_resetn = #(C_DELAY) sys_resetn;
   assign hr_csn = #(C_DELAY) sys_csn;
   assign hr_ck = #(C_DELAY) sys_ck;

   // Instantiate wiredelay2 module for RWDS
   work_wiredelay2 i_wiredelay2_rwds (
      .A(sys_rwds),
      .B(hr_rwds),
      .G_DELAY(C_DELAY)
   );

   // Instantiate wiredelay2 module for DQs
   genvar i;
   generate
      for (i = 0; i < 8; i = i + 1) begin : gen_dq_delay
         work_wiredelay2 i_wiredelay2_rwds (
            .A(sys_dq[i]),
            .B(hr_dq[i]),
            .G_DELAY(C_DELAY)
         );
      end
   endgenerate

   // Instantiate HyperRAM simulation model
   work_s27kl0642 i_s27kl0642 (
      .DQ7(hr_dq[7]),
      .DQ6(hr_dq[6]),
      .DQ5(hr_dq[5]),
      .DQ4(hr_dq[4]),
      .DQ3(hr_dq[3]),
      .DQ2(hr_dq[2]),
      .DQ1(hr_dq[1]),
      .DQ0(hr_dq[0]),
      .RWDS(hr_rwds),
      .CSNeg(hr_csn),
      .CK(sys_ck),
      .CKn(~sys_ck),
      .RESETNeg(hr_resetn)
   );


// Main test process
      always @(posedge clk) begin
          std_logic_vector[G_DATA_SIZE-1:0] read_bus_d;
          integer ncycles;

          test_runner_setup(runner, RUNNER_CFG);

          // Initialize to same seed to get the same sequence
          rnd_stimuli.InitSeed(rnd_stimuli'instance_name);
          rnd_expected.InitSeed(rnd_stimuli'instance_name);

          while (test_suite) begin
              if (run("bulk_write_bulk_read_operation")) begin
                  #1; // Wait for 1 us
                  for (integer write_count=0; write_count<1024; write_count=write_count+1) begin
                      avm_rnd_addr = rnd_stimuli.RandSlv(0, 2**23, avm_rnd_addr'length);
                      avm_rnd_data = rnd_stimuli.RandSlv(0, 2**15, avm_rnd_data'length);
                      #10; // Wait for 10 ns
                      write_bus(net, AVALON_BUS, avm_rnd_addr, avm_rnd_data);
                      #300; // Wait for 300 ns
                  end
                  for (integer read_count=0; read_count<1024; read_count=read_count+1) begin
                      avm_rnd_addr = rnd_expected.RandSlv(0, 2**23, avm_rnd_addr'length);
                      avm_rnd_data = rnd_expected.RandSlv(0, 2**15, avm_rnd_data'length);
                      #10; // Wait for 10 ns
                      read_bus(net, AVALON_BUS, avm_rnd_addr, read_bus_d);
                      check_equal(read_bus_d, avm_rnd_data);
                      #50; // Wait for 50 ns
                  end
                  test_runner_cleanup(runner);
              end

              if (run("seq_write_read_operation")) begin
                  #1; // Wait for 1 us
                  for (integer i=0; i<8192; i=i+1) begin
                      avm_rnd_addr = rnd_stimuli.RandSlv(0, 2**23, avm_rnd_addr'length);
                      avm_rnd_data = rnd_stimuli.RandSlv(0, 2**15, avm_rnd_data'length);
                      #10; // Wait for 10 ns
                      write_bus(net, AVALON_BUS, avm_rnd_addr, avm_rnd_data);
                      #10; // Wait for 10 ns
                      read_bus(net, AVALON_BUS, avm_rnd_addr, read_bus_d);
                      check_equal(read_bus_d, avm_rnd_data);
                      #200; // Wait for 200 ns
                      ncycles = ncycles + 1;
                  end
                  $display("All tests are completed");
                  test_runner_cleanup(runner);
              end
          end
      end

   //!

endmodule

