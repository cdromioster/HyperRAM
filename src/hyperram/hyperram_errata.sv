// This module provides a work-around for a bug in some newer HyperRAM devices,
// specifically ISSI revision D dies.
//
// See errata here: https://issi.com/WW/pdf/appnotes/sram/AN66WX001_VariableMode_MinimumDataSize.pdf
//
// Created by Michael Jørgensen in 2023

module hyperram_errata (
    input wire clk_i,
    input wire rst_i,
    output wire s_avm_waitrequest_o,
    input wire s_avm_write_i,
    input wire s_avm_read_i,
    input wire [31:0] s_avm_address_i,
    input wire [15:0] s_avm_writedata_i,
    input wire [1:0] s_avm_byteenable_i,
    input wire [7:0] s_avm_burstcount_i,
    output wire [15:0] s_avm_readdata_o,
    output wire s_avm_readdatavalid_o,
    input wire m_avm_waitrequest_i,
    output wire m_avm_write_o,
    output wire m_avm_read_o,
    output wire [31:0] m_avm_address_o,
    output wire [15:0] m_avm_writedata_o,
    output wire [1:0] m_avm_byteenable_o,
    output wire [7:0] m_avm_burstcount_o,
    input wire [15:0] m_avm_readdata_i,
    input wire m_avm_readdatavalid_i
);
    
    typedef enum logic {
        NORMAL_ST = 1'b0,
        ERRATA_ST = 1'b1
    } t_state;

    t_state state;

    assign s_avm_waitrequest_o = (state == ERRATA_ST) ? 1'b1 : m_avm_waitrequest_i;
    assign s_avm_readdata_o = m_avm_readdata_i;
    assign s_avm_readdatavalid_o = m_avm_readdatavalid_i;

    assign m_avm_write_o = (state == ERRATA_ST) ? 1'b1 : s_avm_write_i;
    assign m_avm_read_o = (state == ERRATA_ST) ? 1'b0 : s_avm_read_i;
    assign m_avm_address_o = s_avm_address_i;
    assign m_avm_writedata_o = s_avm_writedata_i;
    assign m_avm_byteenable_o = (state == ERRATA_ST) ? 2'b00 : s_avm_byteenable_i;
    assign m_avm_burstcount_o = (s_avm_waitrequest_o == 1'b0 && s_avm_write_i == 1'b1 && s_avm_burstcount_i == 8'h01 && state == NORMAL_ST) ? 8'h02 : s_avm_burstcount_i;

    always @(posedge clk_i or posedge rst_i) begin
        if (rst_i) begin
            state <= NORMAL_ST;
        end else begin
            case (state)
                NORMAL_ST:
                    if (s_avm_waitrequest_o == 1'b0 && s_avm_write_i == 1'b1 && s_avm_burstcount_i == 8'h01) begin
                        state <= ERRATA_ST;
                    end
                ERRATA_ST:
                    if (m_avm_waitrequest_i == 1'b0) begin
                        state <= NORMAL_ST;
                    end
            endcase
        end
    end
    
endmodule
