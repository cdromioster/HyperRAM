// This is the HyperRAM I/O connections
// The additional clock clk_x2_i is used to drive the DQ/RWDS output and to
// sample the DQ/RWDS input.
// The additional clock clk_x2_del_i is used to drive the CK output.
//
// Created by Michael Jørgensen in 2022 (mjoergen.github.io/HyperRAM).

module hyperram_io(
    input wire clk_x1_i,
    input wire clk_x2_i, // Double frequency.
    input wire clk_x2_del_i, // Double frequency, phase shifted.
    input wire rst_i,

    // Connect to HyperRAM controller
    input wire ctrl_rstn_i,
    input wire [1:0] ctrl_ck_ddr_i,
    input wire ctrl_csn_i,
    output logic [15:0] ctrl_dq_ddr_in_o,
    input wire [15:0] ctrl_dq_ddr_out_i,
    input wire ctrl_dq_oe_i,
    output logic ctrl_dq_ie_o,
    input wire [1:0] ctrl_rwds_ddr_out_i,
    input wire ctrl_rwds_oe_i,

    // Connect to HyperRAM device
    output wire hr_resetn_o,
    output wire hr_csn_o,
    output logic hr_ck_o,
    input wire hr_rwds_in_i,
    input wire [7:0] hr_dq_in_i,
    output logic hr_rwds_out_o,
    output logic [7:0] hr_dq_out_o,
    output logic hr_rwds_oe_o,
    output logic hr_dq_oe_o
);

    // Output generation
    reg [1:0] rwds_ddr_out_x2;
    reg [15:0] dq_ddr_out_x2;

    // Input sampling
    reg csn_in_x2;
    reg rwds_in_x2;
    reg [7:0] dq_in_x2;
    reg rwds_in_x2_d;
    reg [7:0] dq_in_x2_d;

    assign hr_csn_o = ctrl_csn_i;
    assign hr_resetn_o = ctrl_rstn_i;

    // Output generation
    // Note the use of clk_x2_del_i
    always @(posedge clk_x2_del_i) begin
        if (hr_ck_o == 1'b0) begin
            hr_ck_o <= ctrl_ck_ddr_i[1];
        end else begin
            hr_ck_o <= 1'b0;
        end
    end

    always @(posedge clk_x2_i) begin
        if (hr_ck_o == 1'b0) begin
            rwds_ddr_out_x2 <= ctrl_rwds_ddr_out_i;
            hr_rwds_out_o <= rwds_ddr_out_x2[1];
        end else begin
            hr_rwds_out_o <= rwds_ddr_out_x2[0];
        end

        if (hr_ck_o == 1'b0) begin
            dq_ddr_out_x2 <= ctrl_dq_ddr_out_i;
            hr_dq_out_o <= dq_ddr_out_x2[15:8];
        end else begin
            hr_dq_out_o <= dq_ddr_out_x2[7:0];
        end
    end

    always @(posedge clk_x1_i) begin
        hr_dq_oe_o <= ctrl_dq_oe_i;
        hr_rwds_oe_o <= ctrl_rwds_oe_i;
    end

    // Input sampling
    always @(posedge clk_x2_i) begin
        csn_in_x2 <= hr_csn_o;
        rwds_in_x2 <= hr_rwds_in_i;
        dq_in_x2 <= hr_dq_in_i;
        rwds_in_x2_d <= rwds_in_x2;
        dq_in_x2_d <= dq_in_x2;
    end

    always @(posedge clk_x1_i) begin
        ctrl_dq_ie_o <= 1'b0;
        if ((rwds_in_x2_d == 1'b1) && (rwds_in_x2 == 1'b0)) begin
            ctrl_dq_ddr_in_o <= {dq_in_x2_d, dq_in_x2};
            ctrl_dq_ie_o <= 1'b1;
        end
        if ((rwds_in_x2_d == 1'b0) && (rwds_in_x2 == 1'b1)) begin
            ctrl_dq_ddr_in_o <= {dq_in_x2, hr_dq_in_i};
            ctrl_dq_ie_o <= 1'b1;
        end
    end

endmodule
