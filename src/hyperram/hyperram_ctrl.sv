// This is the main state machine of the HyperRAM controller.
// The purpose is to implement the HyperBus protocol, i.e.
// to decode the Avalon Memory Map requests and generate the control
// signals for the HyperRAM device.
//
// Bit 31 of avm_address_i is used to indicate register space.
//
// Created by Michael Jørgensen in 2022 (mjoergen.github.io/HyperRAM).

module hyperram_ctrl (
    input wire clk_i,
    input wire rst_i,

    // Avalon Memory Map
    input wire avm_write_i,
    input wire avm_read_i,
    input wire [31:0] avm_address_i,
    input wire [15:0] avm_writedata_i,
    input wire [1:0] avm_byteenable_i,
    input wire [7:0] avm_burstcount_i,
    output logic [15:0] avm_readdata_o,
    output logic avm_readdatavalid_o,
    output logic avm_waitrequest_o,
    // HyperBus control signals
    output logic hb_rstn_o,
    output wire [1:0] hb_ck_ddr_o,
    output logic hb_csn_o,
    input wire [15:0] hb_dq_ddr_in_i,
    output wire [15:0] hb_dq_ddr_out_o,
    output logic hb_dq_oe_o,
    input wire hb_dq_ie_i,
    output wire [1:0] hb_rwds_ddr_out_o,
    output wire hb_rwds_oe_o,
    input wire hb_rwds_in_i
);
    
    typedef enum logic [2:0] {
        INIT_ST = 3'b000,
        COMMAND_ADDRESS_ST = 3'b001,
        LATENCY_ST = 3'b010,
        READ_ST = 3'b011,
        WRITE_ST = 3'b100,
        WRITE_BURST_ST = 3'b101,
        RECOVERY_ST = 3'b110
    } state_t;

    state_t state;

    logic [15:0] writedata;
    logic [1:0] byteenable;
    logic read;
    logic config_bit;
    logic [7:0] burst_count;
    logic [47:0] command_address;
    logic [1:0] ca_count;
    logic [3:0] latency_count;
    logic [7:0] read_clk_count;
    logic [7:0] read_return_count;
    logic [7:0] write_clk_count;
    logic [7:0] recovery_count;

    // Decode Command/Address value
    parameter R_CA_RW = 47;
    parameter R_CA_AS = 46;
    parameter R_CA_BURST = 45;
    parameter R_CA_ADDR_MSB = 44;
    parameter R_CA_RESERVED = 15;
    parameter R_CA_ADDR_LSB = 2;

    parameter G_LATENCY = 4;

    always @(posedge clk_i) begin
        avm_readdatavalid_o <= 1'b0;
        hb_rstn_o <= 1'b1;
        hb_dq_oe_o <= 1'b0;
        hb_rwds_oe_o <= 1'b0;

        case (state)
            INIT_ST:
                begin
                    if (avm_read_i == 1'b1 || avm_write_i == 1'b1) begin
                        read        <= avm_read_i;
                        config_bit  <= avm_address_i[31];
                        writedata   <= avm_writedata_i;
                        byteenable  <= avm_byteenable_i;
                        burst_count <= avm_burstcount_i;

                        command_address[R_CA_RW] <= avm_read_i;
                        command_address[R_CA_AS] <= avm_address_i[31];
                        command_address[R_CA_BURST] <= 1'b1;
                        command_address[R_CA_ADDR_MSB -: 29] <= {1'b0, avm_address_i[30:3]};
                        command_address[R_CA_RESERVED -: 13] <= 13'b0;
                        command_address[R_CA_ADDR_LSB -: 3] <= avm_address_i[2:0];

                        avm_waitrequest_o   <= 1'b1;
                        hb_csn_o            <= 1'b0;
                        hb_dq_oe_o          <= 1'b1;
                        ca_count            <= 2'b10;
                        if (avm_address_i[31] == 1'b1) begin
                            ca_count <= 2'b11;
                        end
                        state <= COMMAND_ADDRESS_ST;
                    end
                end
            
            COMMAND_ADDRESS_ST:
                begin
                    command_address[R_CA_RW:0] <= {command_address[31:0], writedata[15:0]};

                    if (ca_count > 0) begin
                        hb_dq_oe_o  <= 1'b1;
                        hb_ck_ddr_o <= 2'b10;
                        ca_count    <= ca_count - 1;
                    end else begin
                        if (hb_rwds_in_i == 1'b1) begin
                            latency_count <= 2 * G_LATENCY - 2;
                        end else begin
                            latency_count <= G_LATENCY - 2;
                        end
                        if (config_bit == 1'b1 && read == 1'b0) begin
                            recovery_count <= 3;
                            state <= RECOVERY_ST;
                        end else begin
                            state <= LATENCY_ST;
                        end
                    end
                end
            
            LATENCY_ST:
                begin
                    if (latency_count > 0) begin
                        latency_count <= latency_count - 1;
                    end else begin
                        if (read == 1'b1) begin
                            read_clk_count    <= burst_count;
                            read_return_count <= burst_count;
                            state             <= READ_ST;
                        end else begin
                            write_clk_count   <= burst_count;
                            hb_dq_oe_o        <= 1'b1;
                            hb_rwds_oe_o      <= 1'b1;
                            state <= WRITE_ST;
                        end
                    end
                end
            
            READ_ST:
                begin
                    if (read_clk_count > 0) begin
                        read_clk_count <= read_clk_count - 1;
                    end else begin
                        hb_ck_ddr_o <= 2'b00;
                    end

                    if (hb_dq_ie_i == 1'b1) begin
                        avm_readdata_o      <= hb_dq_ddr_in_i;
                        avm_readdatavalid_o <= 1'b1;
                        read_return_count   <= read_return_count - 1;
                        if (read_return_count == 8'd1) begin
                            hb_csn_o        <= 1'b1;
                            hb_ck_ddr_o     <= 2'b00;
                            recovery_count  <= 3;
                            state <= RECOVERY_ST;
                        end
                    end
                end
            
            WRITE_ST, WRITE_BURST_ST:
                begin
                    state             <= WRITE_BURST_ST;
                    writedata         <= avm_writedata_i;
                    byteenable        <= avm_byteenable_i;
                    hb_dq_oe_o        <= 1'b1;
                    hb_rwds_oe_o      <= 1'b1;
                    avm_waitrequest_o <= 1'b0;

                    if (avm_write_i == 1'b1 || state == WRITE_ST) begin
                        hb_ck_ddr_o <= 2'b10;
                        if (write_clk_count > 0) begin
                            write_clk_count <= write_clk_count - 1;
                            if (write_clk_count == 8'd1) begin
                                recovery_count      <= 4;
                                hb_dq_oe_o          <= 1'b0;
                                hb_rwds_oe_o        <= 1'b0;
                                avm_waitrequest_o   <= 1'b1;
                                state <= RECOVERY_ST;
                            end
                        end
                    end else begin
                        hb_ck_ddr_o <= 2'b00;
                    end
                end
            
            //RECOVERY_ST:
            default:
                begin
                    hb_csn_o <= 1'b1;
                    hb_ck_ddr_o <= 2'b00;
                    if (recovery_count > 0) begin
                        recovery_count <= recovery_count - 1;
                    end else begin
                        avm_waitrequest_o <= 1'b0;
                        state <= INIT_ST;
                    end
                end
        endcase

        if (rst_i == 1'b1) begin
            avm_waitrequest_o   <= 1'b1;
            avm_readdatavalid_o <= 1'b0;
            state               <= INIT_ST;
            hb_rstn_o           <= 1'b0;
            hb_ck_ddr_o         <= 2'b00;
            hb_csn_o            <= 1'b1;
            hb_dq_oe_o          <= 1'b0;
            hb_rwds_oe_o        <= 1'b0;
        end
    end

    assign hb_dq_ddr_out_o = (state == WRITE_BURST_ST) ? avm_writedata_i : command_address[47:32];
    assign hb_rwds_ddr_out_o = (state == WRITE_ST || state == WRITE_BURST_ST) ? ~byteenable : 2'b00;

endmodule
