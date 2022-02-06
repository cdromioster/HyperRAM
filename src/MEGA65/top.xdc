## Signal mapping for MEGA65-R3

#############################################################################################################
# Pin locations and I/O standards
#############################################################################################################

## External clock signal (connected to 100 MHz oscillator)
set_property -dict {PACKAGE_PIN V13  IOSTANDARD LVCMOS33}                                    [get_ports {clk}]

## Reset signal (Active low. From MAX10)
set_property -dict {PACKAGE_PIN M13  IOSTANDARD LVCMOS33}                                    [get_ports {reset_n}]

# HDMI output
set_property -dict {PACKAGE_PIN Y1   IOSTANDARD TMDS_33}  [get_ports {hdmi_clk_n}]
set_property -dict {PACKAGE_PIN W1   IOSTANDARD TMDS_33}  [get_ports {hdmi_clk_p}]
set_property -dict {PACKAGE_PIN AB1  IOSTANDARD TMDS_33}  [get_ports {hdmi_data_n[0]}]
set_property -dict {PACKAGE_PIN AA1  IOSTANDARD TMDS_33}  [get_ports {hdmi_data_p[0]}]
set_property -dict {PACKAGE_PIN AB2  IOSTANDARD TMDS_33}  [get_ports {hdmi_data_n[1]}]
set_property -dict {PACKAGE_PIN AB3  IOSTANDARD TMDS_33}  [get_ports {hdmi_data_p[1]}]
set_property -dict {PACKAGE_PIN AB5  IOSTANDARD TMDS_33}  [get_ports {hdmi_data_n[2]}]
set_property -dict {PACKAGE_PIN AA5  IOSTANDARD TMDS_33}  [get_ports {hdmi_data_p[2]}]

## HyperRAM (connected to IS66WVH8M8BLL-100B1LI, 64 Mbit, 100 MHz, 3.0 V, single-ended clock)
set_property -dict {PACKAGE_PIN B22  IOSTANDARD LVCMOS33  PULLUP FALSE}                      [get_ports {hr_resetn}]
set_property -dict {PACKAGE_PIN C22  IOSTANDARD LVCMOS33  PULLUP FALSE}                      [get_ports {hr_csn}]
set_property -dict {PACKAGE_PIN D22  IOSTANDARD LVCMOS33  PULLUP FALSE  SLEW FAST  DRIVE 16} [get_ports {hr_ck}]
set_property -dict {PACKAGE_PIN B21  IOSTANDARD LVCMOS33  PULLUP FALSE  SLEW FAST  DRIVE 16} [get_ports {hr_rwds}]
set_property -dict {PACKAGE_PIN A21  IOSTANDARD LVCMOS33  PULLUP FALSE  SLEW FAST  DRIVE 16} [get_ports {hr_dq[0]}]
set_property -dict {PACKAGE_PIN D21  IOSTANDARD LVCMOS33  PULLUP FALSE  SLEW FAST  DRIVE 16} [get_ports {hr_dq[1]}]
set_property -dict {PACKAGE_PIN C20  IOSTANDARD LVCMOS33  PULLUP FALSE  SLEW FAST  DRIVE 16} [get_ports {hr_dq[2]}]
set_property -dict {PACKAGE_PIN A20  IOSTANDARD LVCMOS33  PULLUP FALSE  SLEW FAST  DRIVE 16} [get_ports {hr_dq[3]}]
set_property -dict {PACKAGE_PIN B20  IOSTANDARD LVCMOS33  PULLUP FALSE  SLEW FAST  DRIVE 16} [get_ports {hr_dq[4]}]
set_property -dict {PACKAGE_PIN A19  IOSTANDARD LVCMOS33  PULLUP FALSE  SLEW FAST  DRIVE 16} [get_ports {hr_dq[5]}]
set_property -dict {PACKAGE_PIN E21  IOSTANDARD LVCMOS33  PULLUP FALSE  SLEW FAST  DRIVE 16} [get_ports {hr_dq[6]}]
set_property -dict {PACKAGE_PIN E22  IOSTANDARD LVCMOS33  PULLUP FALSE  SLEW FAST  DRIVE 16} [get_ports {hr_dq[7]}]

## Keyboard interface (connected to MAX10)
set_property -dict {PACKAGE_PIN A14  IOSTANDARD LVCMOS33}                                    [get_ports {kb_io0}]
set_property -dict {PACKAGE_PIN A13  IOSTANDARD LVCMOS33}                                    [get_ports {kb_io1}]
set_property -dict {PACKAGE_PIN C13  IOSTANDARD LVCMOS33}                                    [get_ports {kb_io2}]


############################################################################################################
# Clocks
#############################################################################################################

create_clock -period 10.000 -name clk [get_ports clk]

## name autogenerated clocks
## (Plus: using them in separate statements, e.g. clock dividers requries that they have been defined here,
## otherwise Vivado does not find the pins)
create_generated_clock -name clk_x2     [get_pins i_clk/i_clk_hyperram/CLKOUT1]
create_generated_clock -name clk_x2_del [get_pins i_clk/i_clk_hyperram/CLKOUT2]
create_generated_clock -name clk_x1     [get_pins i_clk/i_clk_hyperram/CLKOUT3]
create_generated_clock -name kbd_clk    [get_pins i_clk_mega65/i_clk_mega65/CLKOUT0]
create_generated_clock -name pixel_clk  [get_pins i_clk_mega65/i_clk_mega65/CLKOUT1]
create_generated_clock -name pixel_clk5 [get_pins i_clk_mega65/i_clk_mega65/CLKOUT2]

# MEGA65 timing
set_false_path -from [get_ports reset_n]
set_false_path   -to [get_ports hdmi_data_p[*]]
set_false_path   -to [get_ports hdmi_clk_p]
set_false_path   -to [get_ports kb_io0]
set_false_path   -to [get_ports kb_io1]
set_false_path -from [get_ports kb_io2]


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

