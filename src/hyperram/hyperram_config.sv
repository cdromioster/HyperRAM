// This is the HyperRAM "configurator".
// It performs two functions:
// * Wait until the HyperRAM device is operational after reset.
// * Perform a write to configuration register 0 to set latency mode.
//
// Created by Michael Jørgensen in 2022 (mjoergen.github.io/HyperRAM).

module hyperram_config (
    input wire clk_i,
    input wire rst_i,

    // Slave interface (input). Connect to client.
    input wire s_avm_write_i,
    input wire s_avm_read_i,
    input wire [31:0] s_avm_address_i,
    input wire [15:0] s_avm_writedata_i,
    input wire [1:0] s_avm_byteenable_i,
    input wire [7:0] s_avm_burstcount_i,
    output wire [15:0] s_avm_readdata_o,
    output wire s_avm_readdatavalid_o,
    output wire s_avm_waitrequest_o,

    // Master interface (output). Connect to controller.
    output wire m_avm_write_o,
    output wire m_avm_read_o,
    output wire [31:0] m_avm_address_o,
    output wire [15:0] m_avm_writedata_o,
    output wire [1:0] m_avm_byteenable_o,
    output wire [7:0] m_avm_burstcount_o,
    input wire [15:0] m_avm_readdata_i,
    input wire m_avm_readdatavalid_i,
    input wire m_avm_waitrequest_i
);
    
    parameter C_INIT_DELAY = 150 * 100; // 150 us @ 100 MHz.
    parameter G_LATENCY = 4;

    // Decode configuration register 0
    parameter R_C0_DPD = 15;
    parameter R_C0_DRIVE = 14;//14:12
    parameter R_C0_RESERVED = 11;//11:8
    parameter R_C0_LATENCY = 7;//7:4
    parameter R_C0_FIXED = 3;
    parameter R_C0_HYBRID = 2;
    parameter R_C0_BURST = 1;//1:0

    typedef enum logic [1:0] {
        INIT_ST = 2'b00,
        CONFIG_ST = 2'b01,
        READY_ST = 2'b10
    } state_t;

    state_t state;

    logic [31:0] init_counter;
    logic [15:0] cfg_readdata;
    logic cfg_readdatavalid;
    logic cfg_waitrequest;
    logic cfg_write;
    logic cfg_read;
    logic [31:0] cfg_address;
    logic [15:0] cfg_writedata;
    logic [1:0] cfg_byteenable;
    logic [7:0] cfg_burstcount;

    always @(posedge clk_i) begin
        if (m_avm_waitrequest_i == 1'b0) begin
            cfg_write <= 1'b0;
            cfg_read  <= 1'b0;
        end

        case (state)
            INIT_ST:
                begin
                    if (init_counter > 0) begin
                        init_counter <= init_counter - 1;
                    end else begin
                        $display("Init completed");
                        state <= CONFIG_ST;
                    end
                end
            
            CONFIG_ST:
                begin
                    // Write to configuration register 0
                    // See section 5.2.1 of the datasheet.
                    // The drive strength is chosen to the lowest resistance, i.e. strongest drive strength.
                    // This improves signal quality (lower rise and fall times) and therefore larger timing margin.
                    // The latency mode is set to variable. This results in lower latency on average.
                    cfg_write       <= 1'b1;
                    cfg_read        <= 1'b0;
                    cfg_address     <= 32'b0;
                    cfg_address[18:11] <= 8'h01;
                    cfg_address[31] <= 1'b1;
                    cfg_writedata[R_C0_DPD]      <= 1'b1; // normal (default)
                    cfg_writedata[R_C0_DRIVE -: 3]    <= 3'b111; // 19 ohms (maximal drive strength)
                    cfg_writedata[R_C0_RESERVED -: 4] <= 4'b1111;
                    cfg_writedata[R_C0_LATENCY -: 4]  <= $signed(G_LATENCY) - 5;
                    cfg_writedata[R_C0_FIXED]    <= 1'b0; // variable
                    cfg_writedata[R_C0_HYBRID]   <= 1'b1; // legacy (default)
                    cfg_writedata[R_C0_BURST -: 2]    <= 2'b11; // 32 bytes (default)
                    cfg_byteenable  <= 2'b11;
                    cfg_burstcount  <= 8'h01;
    
                    if (cfg_write == 1'b1 && m_avm_waitrequest_i == 1'b0) begin
                        state <= READY_ST;
                    end
                end
            
            READY_ST:
                begin
                    // Stay here forever after (or until reset)
                end
        endcase

        if (rst_i == 1'b1) begin
            cfg_write    <= 1'b0;
            cfg_read     <= 1'b0;
            init_counter <= C_INIT_DELAY;
            state        <= INIT_ST;
        end
    end

    assign m_avm_write_o = (state == READY_ST) ? s_avm_write_i : cfg_write;
    assign m_avm_read_o = (state == READY_ST) ? s_avm_read_i : cfg_read;
    assign m_avm_address_o = (state == READY_ST) ? s_avm_address_i : cfg_address;
    assign m_avm_writedata_o = (state == READY_ST) ? s_avm_writedata_i : cfg_writedata;
    assign m_avm_byteenable_o = (state == READY_ST) ? s_avm_byteenable_i : cfg_byteenable;
    assign m_avm_burstcount_o = (state == READY_ST) ? s_avm_burstcount_i : cfg_burstcount;
    assign s_avm_readdata_o = (state == READY_ST) ? m_avm_readdata_i : 16'h0;
    assign s_avm_readdatavalid_o = (state == READY_ST) ? m_avm_readdatavalid_i : 1'b0;
    assign s_avm_waitrequest_o = (state == READY_ST) ? m_avm_waitrequest_i : 1'b1;

endmodule
