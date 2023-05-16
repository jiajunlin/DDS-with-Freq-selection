
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.all;

entity dds_w_freq_select_tb is
	-- Generic declarations of the tested unit
		generic(
		a : positive := 14;
		m : positive := 7 );
end dds_w_freq_select_tb;

architecture tb_architecture of dds_w_freq_select_tb is

	-- Stimulus signals - signals mapped to the input and inout ports of tested entity
	signal clk : std_logic := '0';
	signal reset_bar : std_logic;
	signal freq_val : std_logic_vector(a-1 downto 0);
	signal load_freq : std_logic;
	-- Observed signals - signals mapped to the output ports of tested entity
	signal dac_sine_value : std_logic_vector(7 downto 0);
	signal pos : std_logic;
	constant period : time := 1 us;

begin
	-- Unit Under Test port map
	UUT : entity dds_w_freq_select
		generic map (
			a => a,
			m => m
		)

		port map (
			clk => clk,
			reset_bar => reset_bar,
			freq_val => freq_val,
			load_freq => load_freq,
			dac_sine_value => dac_sine_value, 
			pos_sine => pos
		);

	--freq_val <= "00000000000010";
	freq_val <= "01000000000000";
	
	load_freq <= '0', '1' after 7 * period, '0' after 10 * period;
		
	reset_bar <= '0', '1' after 4 * period;	-- reset signal
	
	clock: process				-- system clock
	begin
		for i in 0 to 1032 * (2 ** 7) loop
			wait for period/2;
			clk <= not clk;
		end loop;
		std.env.finish;
	end process;
end tb_architecture;



