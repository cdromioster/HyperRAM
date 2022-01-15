## Signal mapping for MEGA65-R3

#############################################################################################################
# Pin locations and I/O standards
#############################################################################################################

## External clock signal (connected to 100 MHz oscillator)
set_property -dict {PACKAGE_PIN V13  IOSTANDARD LVCMOS33}                                    [get_ports {clk}]

## Reset signal (Active low. From MAX10)
set_property -dict {PACKAGE_PIN M13  IOSTANDARD LVCMOS33}                                    [get_ports {reset_n}]

## Debug LED (Active high. Connected to D9)
set_property -dict {PACKAGE_PIN U22  IOSTANDARD LVCMOS33}                                    [get_ports {uled}]

## HyperRAM (connected to IS66WVH8M8BLL-100B1LI, 64 Mbit, 100 MHz, 3.0 V)
set_property -dict {PACKAGE_PIN B22  IOSTANDARD LVCMOS33  PULLUP FALSE}                      [get_ports {hr_resetn}]
set_property -dict {PACKAGE_PIN C22  IOSTANDARD LVCMOS33  PULLUP FALSE}                      [get_ports {hr_csn}]
set_property -dict {PACKAGE_PIN D22  IOSTANDARD LVCMOS33  PULLUP FALSE  SLEW FAST  DRIVE 16} [get_ports {hr_ck}] # Single-ended
set_property -dict {PACKAGE_PIN B21  IOSTANDARD LVCMOS33  PULLUP FALSE  SLEW FAST  DRIVE 16} [get_ports {hr_rwds}]
set_property -dict {PACKAGE_PIN A21  IOSTANDARD LVCMOS33  PULLUP FALSE  SLEW FAST  DRIVE 16} [get_ports {hr_dq[0]}]
set_property -dict {PACKAGE_PIN D21  IOSTANDARD LVCMOS33  PULLUP FALSE  SLEW FAST  DRIVE 16} [get_ports {hr_dq[1]}]
set_property -dict {PACKAGE_PIN C20  IOSTANDARD LVCMOS33  PULLUP FALSE  SLEW FAST  DRIVE 16} [get_ports {hr_dq[2]}]
set_property -dict {PACKAGE_PIN A20  IOSTANDARD LVCMOS33  PULLUP FALSE  SLEW FAST  DRIVE 16} [get_ports {hr_dq[3]}]
set_property -dict {PACKAGE_PIN B20  IOSTANDARD LVCMOS33  PULLUP FALSE  SLEW FAST  DRIVE 16} [get_ports {hr_dq[4]}]
set_property -dict {PACKAGE_PIN A19  IOSTANDARD LVCMOS33  PULLUP FALSE  SLEW FAST  DRIVE 16} [get_ports {hr_dq[5]}]
set_property -dict {PACKAGE_PIN E21  IOSTANDARD LVCMOS33  PULLUP FALSE  SLEW FAST  DRIVE 16} [get_ports {hr_dq[6]}]
set_property -dict {PACKAGE_PIN E22  IOSTANDARD LVCMOS33  PULLUP FALSE  SLEW FAST  DRIVE 16} [get_ports {hr_dq[7]}]


############################################################################################################
# Clocks and timing
#############################################################################################################

create_clock -period 10.000 -name clk [get_ports clk] # 100 MHz

## name autogenerated clocks
## (Plus: using them in separate statements, e.g. clock dividers requries that they have been defined here,
## otherwise Vivado does not find the pins)
create_generated_clock -name clk_x4  [get_pins i_clk/i_clk_x4_90/CLKOUT0]
create_generated_clock -name clk_90  [get_pins i_clk/i_clk_x4_90/CLKOUT1]
create_generated_clock -name rwds_x4 -source [get_pins i_clk/i_clk_x4_90/CLKOUT0] -divide_by 1 [get_pins i_system/i_hyperram/i_hyperram_io/rwds_x4_reg/Q]


#############################################################################################################
# Configuration and Bitstream properties
#############################################################################################################

set_property CONFIG_VOLTAGE                  3.3   [current_design]
set_property CFGBVS                          VCCO  [current_design]
set_property BITSTREAM.GENERAL.COMPRESS      TRUE  [current_design]
set_property BITSTREAM.CONFIG.CONFIGRATE     66    [current_design]
set_property CONFIG_MODE                     SPIx4 [current_design]
set_property BITSTREAM.CONFIG.SPI_32BIT_ADDR YES   [current_design]
set_property BITSTREAM.CONFIG.SPI_BUSWIDTH   4     [current_design]

