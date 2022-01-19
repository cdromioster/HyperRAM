library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.video_modes_pkg.all;
use work.types_pkg.all;

library xpm;
use xpm.vcomponents.all;

entity top is
   port (
      clk       : in    std_logic;                  -- 100 MHz clock
      reset_n   : in    std_logic;                  -- CPU reset button

      -- Digital Video
      hdmi_data_p : out std_logic_vector(2 downto 0);
      hdmi_data_n : out std_logic_vector(2 downto 0);
      hdmi_clk_p  : out std_logic;
      hdmi_clk_n  : out std_logic;

      -- HyperRAM device interface
      hr_resetn : out   std_logic;
      hr_csn    : out   std_logic;
      hr_ck     : out   std_logic;
      hr_rwds   : inout std_logic;
      hr_dq     : inout std_logic_vector(7 downto 0);

      -- Interface for physical keyboard
      kb_io0    : out   std_logic;
      kb_io1    : out   std_logic;
      kb_io2    : in    std_logic
   );
end entity top;

architecture synthesis of top is

   -- video mode selection: 720p @ 60 Hz
   constant C_VIDEO_MODE : video_modes_t := C_VIDEO_MODE_1280_720_60;
   constant C_FONT_FILE  : string := "font8x8.txt";

   ---------------------------------------------------------------------------------------------
   -- video_clk
   ---------------------------------------------------------------------------------------------

   signal hdmi_data   : slv_9_0_t(0 to 2);              -- parallel HDMI symbol stream x 3 channels
   signal video_vs    : std_logic;
   signal video_hs    : std_logic;
   signal video_de    : std_logic;
   signal video_red   : std_logic_vector(7 downto 0);
   signal video_green : std_logic_vector(7 downto 0);
   signal video_blue  : std_logic_vector(7 downto 0);


   -- clocks
   signal clk_90 : std_logic; -- 90 degrees phase shift
   signal clk_x2 : std_logic; -- Double speed clock
   signal clk_40 : std_logic; -- Keyboard clock

   signal video_clk : std_logic;
   signal hdmi_clk  : std_logic;

   -- resets
   signal rst       : std_logic;
   signal video_rst : std_logic;

   signal return_out  : std_logic;
   signal start       : std_logic;

   signal led_active  : std_logic;
   signal led_error   : std_logic;

   signal address     : std_logic_vector(21 downto 0);
   signal data_exp    : std_logic_vector(15 downto 0);
   signal data_read   : std_logic_vector(15 downto 0);

   signal digits      : std_logic_vector(55 downto 0);

begin

   i_clk_video : entity work.clk_video
      port map
      (
         sys_clk_i    => clk,
         sys_rstn_i   => reset_n,
         pixel_clk_o  => video_clk,
         pixel_rst_o  => video_rst,
         pixel_clk5_o => hdmi_clk
      ); -- i_clk_video

   i_clk : entity work.clk
      port map
      (
         sys_clk_i  => clk,
         sys_rstn_i => reset_n,
         clk_x2_o   => clk_x2,
         clk_90_o   => clk_90,
         clk_40_o   => clk_40,
         rst_o      => rst
      ); -- i_clk

   i_system : entity work.system
      generic map (
         G_ADDRESS_SIZE => 22       -- 4M entries of 16 bits each.
      )
      port map (
         clk_i        => clk,
         clk_90_i     => clk_90,
         clk_x2_i     => clk_x2,
         rst_i        => rst,
         start_i      => start,
         hr_resetn_o  => hr_resetn,
         hr_csn_o     => hr_csn,
         hr_ck_o      => hr_ck,
         hr_rwds_io   => hr_rwds,
         hr_dq_io     => hr_dq,
         address_o    => address,
         data_exp_o   => data_exp,
         data_read_o  => data_read,
         active_o     => led_active,
         error_o      => led_error
      ); -- i_system

   i_cdc: xpm_cdc_array_single                                                                  
      generic map (
         WIDTH => 56
      )
      port map (
         src_clk              => clk,
         src_in(15 downto  0) => data_read,
         src_in(31 downto 16) => data_exp,
         src_in(53 downto 32) => address,
         src_in(55 downto 54) => "00",
         dest_clk             => video_clk,
         dest_out             => digits
      ); -- i_cdc

   i_video : entity work.video
      generic map
      (
         G_FONT_FILE   => C_FONT_FILE,
         G_DIGITS_SIZE => 56,
         G_VIDEO_MODE  => C_VIDEO_MODE
      )
      port map
      (
         rst_i         => video_rst,
         clk_i         => video_clk,
         digits_i      => digits,
         video_vs_o    => video_vs,
         video_hs_o    => video_hs,
         video_de_o    => video_de,
         video_red_o   => video_red,
         video_green_o => video_green,
         video_blue_o  => video_blue
      ); -- i_video


   i_audio_video_to_hdmi : entity work.audio_video_to_hdmi
      port map (
      select_44100 => '0',
      dvi          => '0',
      vic          => std_logic_vector(to_unsigned(4,8)),  -- CEA/CTA VIC 4=720p @ 60 Hz
      aspect       => "10",                                -- 01=4:3, 10=16:9
      pix_rep      => '0',                                 -- no pixel repetition
      vs_pol       => C_VIDEO_MODE.V_POL,                  -- horizontal polarity: positive
      hs_pol       => C_VIDEO_MODE.H_POL,                  -- vertaical polarity: positive

      vga_rst      => video_rst,                           -- active high reset
      vga_clk      => video_clk,                           -- video pixel clock
      vga_vs       => video_vs,
      vga_hs       => video_hs,
      vga_de       => video_de,
      vga_r        => video_red,
      vga_g        => video_green,
      vga_b        => video_blue,

      -- PCM audio
      pcm_rst      => '0',
      pcm_clk      => '0',
      pcm_clken    => '0',

      -- PCM audio is signed
      pcm_l        => X"0000",
      pcm_r        => X"0000",

      pcm_acr      => '0',
      pcm_n        => X"00000",
      pcm_cts      => X"00000",

      -- TMDS output (parallel)
      tmds         => hdmi_data
   ); -- i_audio_video_to_hdmi


   -- serialiser: in this design we use HDMI SelectIO outputs
   gen_hdmi_data: for i in 0 to 2 generate
   begin
      i_serialiser_10to1_selectio_data: entity work.serialiser_10to1_selectio
      port map (
         rst_i    => video_rst,
         clk_i    => video_clk,
         clk_x5_i => hdmi_clk,
         d_i      => hdmi_data(i),
         out_p_o  => hdmi_data_p(i),
         out_n_o  => hdmi_data_n(i)
      ); -- i_serialiser_10to1_selectio_data
   end generate gen_hdmi_data;

   i_serialiser_10to1_selectio_clk : entity work.serialiser_10to1_selectio
   port map (
         rst_i    => video_rst,
         clk_i    => video_clk,
         clk_x5_i => hdmi_clk,
         d_i      => "0000011111",
         out_p_o  => hdmi_clk_p,
         out_n_o  => hdmi_clk_n
      ); -- i_serialiser_10to1_selectio_clk


   i_mega65kbd_to_matrix : entity work.mega65kbd_to_matrix
      port map (
         cpuclock       => clk_40,
         flopled        => led_error,
         powerled       => led_active,
         kio8           => kb_io0,
         kio9           => kb_io1,
         kio10          => kb_io2,
         delete_out     => open,
         return_out     => return_out, -- Active low
         fastkey_out    => open
      ); -- i_mega65kbd_to_matrix

   p_start : process (clk)
   begin
      if rising_edge(clk) then
         start <= not return_out;
      end if;
   end process p_start;

end architecture synthesis;
