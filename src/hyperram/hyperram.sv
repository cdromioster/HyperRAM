// This is the wrapper file for the complete HyperRAM controller.

// Bit 31 of avm_address_i is used to indicate register space.

// The datawidth is fixed at 16 bits.
// The address is word-based, i.e. units of 2 bytes.

// This module requires three clocks:
// clk_x1_i     : 100 MHz : This is the main clock used for the Avalon MM
//                          interface as well as controlling the HyperRAM
//                          device.
// clk_x2_i     :_200 MHz : Used for I/O to HyperRAM device.
// clk_x2_del_i :_200 MHz : Used for I/O to HyperRAM device.
//
// Created by Michael Jørgensen in 2022 (mjoergen.github.io/HyperRAM).

module hyperram #(
    parameter G_ERRATA_ISSI_D_FIX = 1 // Default to true
) (
    input wire clk_x1_i, // Main clock
    input wire clk_x2_i, // Physical I/O only
    input wire clk_x2_del_i, // Double frequency, phase shifted
    input wire rst_i, // Synchronous reset

    // Avalon Memory Map
    input wire avm_write_i,
    input wire avm_read_i,
    input wire [31:0] avm_address_i,
    input wire [15:0] avm_writedata_i,
    input wire [1:0] avm_byteenable_i,
    input wire [7:0] avm_burstcount_i,
    output wire [15:0] avm_readdata_o,
    output wire avm_readdatavalid_o,
    output wire avm_waitrequest_o,

    // HyperRAM device interface
    output wire hr_resetn_o,
    output wire hr_csn_o,
    output wire hr_ck_o,
    input wire hr_rwds_in_i,
    output wire hr_rwds_out_o,
    output wire hr_rwds_oe_o, // Output enable for RWDS
    input wire [7:0] hr_dq_in_i,
    output wire [7:0] hr_dq_out_o,
    output wire hr_dq_oe_o // Output enable for DQ
);

    parameter C_LATENCY = 4;

    wire errata_write;
    wire errata_read;
    wire [31:0] errata_address;
    wire [15:0] errata_writedata;
    wire [1:0] errata_byteenable;
    wire [7:0] errata_burstcount;
    wire [15:0] errata_readdata;
    wire errata_readdatavalid;
    wire errata_waitrequest;

    wire cfg_write;
    wire cfg_read;
    wire [31:0] cfg_address;
    wire [15:0] cfg_writedata;
    wire [1:0] cfg_byteenable;
    wire [7:0] cfg_burstcount;
    wire [15:0] cfg_readdata;
    wire cfg_readdatavalid;
    wire cfg_waitrequest;

    wire ctrl_rstn;
    wire [1:0] ctrl_ck_ddr;
    wire ctrl_csn;
    wire [15:0] ctrl_dq_ddr_in;
    wire [15:0] ctrl_dq_ddr_out;
    wire ctrl_dq_oe;
    wire ctrl_dq_ie;
    wire [1:0] ctrl_rwds_ddr_out;
    wire ctrl_rwds_oe;




    if (G_ERRATA_ISSI_D_FIX) begin
        hyperram_errata #(
            .C_LATENCY(C_LATENCY)
        ) hyperram_errata_inst (
          //------------------------------------------------------
          // Instantiate workaround for errata in ISSI rev D dies
          //--------------------------------------------------------
          .clk_i(clk_x1_i),
          .rst_i(rst_i),
          .s_avm_waitrequest_o(avm_waitrequest_o),
          .s_avm_write_i(avm_write_i),
          .s_avm_read_i(avm_read_i),
          .s_avm_address_i(avm_address_i),
          .s_avm_writedata_i(avm_writedata_i),
          .s_avm_byteenable_i(avm_byteenable_i),
          .s_avm_burstcount_i(avm_burstcount_i),
          .s_avm_readdata_o(avm_readdata_o),
          .s_avm_readdatavalid_o(avm_readdatavalid_o),
          .m_avm_waitrequest_i(errata_waitrequest),
          .m_avm_write_o(errata_write),
          .m_avm_read_o(errata_read),
          .m_avm_address_o(errata_address),
          .m_avm_writedata_o(errata_writedata),
          .m_avm_byteenable_o(errata_byteenable),
          .m_avm_burstcount_o(errata_burstcount),
          .m_avm_readdata_i(errata_readdata),
          .m_avm_readdatavalid_i(errata_readdatavalid)
        );
    end else begin
      //------------------------------------------------------
      // Bypass workaround for errata in ISSI rev D dies
      //------------------------------------------------------
      assign avm_waitrequest_o = errata_waitrequest;
      assign avm_readdata_o = errata_readdata;
      assign avm_readdatavalid_o = errata_readdatavalid;
      assign errata_write = avm_write_i;
      assign errata_read = avm_read_i;
      assign errata_address = avm_address_i;
      assign errata_writedata = avm_writedata_i;
      assign errata_byteenable = avm_byteenable_i;
      assign errata_burstcount = avm_burstcount_i;
    end


    //------------------------------------------------------
    // Instantiate HyperRAM configurator
    //------------------------------------------------------

    hyperram_config #(
        .G_LATENCY(C_LATENCY)
    ) hyperram_config_inst (
        .clk_i(clk_x1_i),
        .rst_i(rst_i),
        .s_avm_write_i(errata_write),
        .s_avm_read_i(errata_read),
        .s_avm_address_i(errata_address),
        .s_avm_writedata_i(errata_writedata),
        .s_avm_byteenable_i(errata_byteenable),
        .s_avm_burstcount_i(errata_burstcount),
        .s_avm_readdata_o(errata_readdata),
        .s_avm_readdatavalid_o(errata_readdatavalid),
        .s_avm_waitrequest_o(errata_waitrequest),
        .m_avm_write_o(cfg_write),
        .m_avm_read_o(cfg_read),
        .m_avm_address_o(cfg_address),
        .m_avm_writedata_o(cfg_writedata),
        .m_avm_byteenable_o(cfg_byteenable),
        .m_avm_burstcount_o(cfg_burstcount),
        .m_avm_readdata_i(cfg_readdata),
        .m_avm_readdatavalid_i(cfg_readdatavalid),
        .m_avm_waitrequest_i(cfg_waitrequest)
    );

    //------------------------------------------------------
    // Instantiate HyperRAM controller
    //------------------------------------------------------

    hyperram_ctrl #(
        .G_LATENCY(C_LATENCY)
    ) hyperram_ctrl_inst (
        .clk_i(clk_x1_i),
        .rst_i(rst_i),
        .avm_write_i(cfg_write),
        .avm_read_i(cfg_read),
        .avm_address_i(cfg_address),
        .avm_writedata_i(cfg_writedata),
        .avm_byteenable_i(cfg_byteenable),
        .avm_burstcount_i(cfg_burstcount),
        .avm_readdata_o(cfg_readdata),
        .avm_readdatavalid_o(cfg_readdatavalid),
        .avm_waitrequest_o(cfg_waitrequest),
        .hb_rstn_o(ctrl_rstn),
        .hb_ck_ddr_o(ctrl_ck_ddr),
        .hb_csn_o(ctrl_csn),
        .hb_dq_ddr_in_i(ctrl_dq_ddr_in),
        .hb_dq_ddr_out_o(ctrl_dq_ddr_out),
        .hb_dq_oe_o(ctrl_dq_oe),
        .hb_dq_ie_i(ctrl_dq_ie),
        .hb_rwds_ddr_out_o(ctrl_rwds_ddr_out),
        .hb_rwds_oe_o(ctrl_rwds_oe),
        .hb_rwds_in_i(hr_rwds_in_i)
    );

    //------------------------------------------------------
    // Instantiate HyperRAM I/O
    //------------------------------------------------------
    
    hyperram_io hyperram_io_inst (
        .clk_x1_i(clk_x1_i),
        .clk_x2_i(clk_x2_i),
        .clk_x2_del_i(clk_x2_del_i),
        .rst_i(rst_i),
        .ctrl_rstn_i(ctrl_rstn),
        .ctrl_ck_ddr_i(ctrl_ck_ddr),
        .ctrl_csn_i(ctrl_csn),
        .ctrl_dq_ddr_in_o(ctrl_dq_ddr_in),
        .ctrl_dq_ddr_out_i(ctrl_dq_ddr_out),
        .ctrl_dq_oe_i(ctrl_dq_oe),
        .ctrl_dq_ie_o(ctrl_dq_ie),
        .ctrl_rwds_ddr_out_i(ctrl_rwds_ddr_out),
        .ctrl_rwds_oe_i(ctrl_rwds_oe),
        .hr_resetn_o(hr_resetn_o),
        .hr_csn_o(hr_csn_o),
        .hr_ck_o(hr_ck_o),
        .hr_rwds_in_i(hr_rwds_in_i),
        .hr_dq_in_i(hr_dq_in_i),
        .hr_rwds_out_o(hr_rwds_out_o),
        .hr_dq_out_o(hr_dq_out_o),
        .hr_rwds_oe_o(hr_rwds_oe_o),
        .hr_dq_oe_o(hr_dq_oe_o)
    );

endmodule
