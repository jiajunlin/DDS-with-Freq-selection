-------------------------------------------------------------------------------
--
-- Title       : Improved DDS with Frequency Select
-- Design      : dds_w_freq_select
-- Author      : Jiajun Lin
-- Company     : Stony Brook University
--
-------------------------------------------------------------------------------
--
-- File        : C:\Users\jlin7\Desktop\ESE 382 Lab 10\dds_w_freq_select\dds_w_freq_select\src\dds_w_freq_select.vhd
-- Generated   : Wed Apr 26 20:10:38 2023
-- From        : interface description file
-- By          : Itf2Vhdl ver. 1.22
--
-------------------------------------------------------------------------------
--
-- Description : This HDL code generates a Sine wave (Improved DDS).
--
-------------------------------------------------------------------------------

--{{ Section below this comment is automatically maintained
--   and may be overwritten
--{entity {dds_w_freq_select} architecture {dds_w_freq_select}}

	
--1
--Edge detection FSM
library IEEE;
use IEEE.std_logic_1164.all; 
use work.all; 

entity edge_det is
port(
rst_bar : in std_logic; -- asynchronous system reset
clk : in std_logic; -- system clock
sig : in std_logic; -- input signal
pos : in std_logic; -- '1' for positive edge, '0' for negative
sig_edge : out std_logic -- high for one sys. clk after edge
);
end edge_det;

--}} End of automatically maintained section

architecture pos_fsm of edge_det is
type state is (state0, state1, state2); 
signal presentS : state := state0 ; 
signal  nextS : state;	

	begin
   	-- if reset is 0, we wait for 0.
		   
	state_reg: process (clk, rst_bar)
	begin 
		if rst_bar = '0' then
			presentS <= state0;
		elsif rising_edge(clk) then --rising clock 
			presentS <= nextS; 		--when there is a rising edge, the next state goes into the present state
		end if; 
		end process;
		
	outputs : process (presentS) 
	begin 
		case presentS is 
			when state2 => sig_edge <= '1';
			when others => sig_edge <='0';
		end case; 
		end process; 
	
	nxt_state : process(presentS, sig)
	begin 
		case presentS is 
		when state0 =>					  --when the present state is in the waitfor 0,
			if (sig = '0') and (pos = '1') then   --when its a positive edge , and input signal is a 1.
				nextS <= state1; 			 
			else
		 	   nextS <= state0; 	  -- it is a positive edge
			end if;
			
			if (sig = '0') and (pos = '0') then 
				nextS <= state1; 			 
			else
		 	   nextS <= state0; 	  -- it is a positive edge
			end if; 
			
		when state1 =>	
		if sig = '1' and pos = '1' then 
			nextS <= state2;
		else
			nextS <= state1;
		end if; 
		if sig = '1' and pos = '0' then 
			nextS <= state2;
		else
			nextS <= state1;
		end if; 
			
		when state2 =>
		if sig = '0' and pos = '1' then 
			nextS <= state1; 
		elsif sig = '0' and pos = '0' then 
			nextS <= state0; 
		end if;
		
		end case; 
	 end process;
			
end pos_fsm;


--2
--Frequency reg
library IEEE;
use IEEE.std_logic_1164.all; 
use IEEE.numeric_std.all; 
use work.all; 

entity frequency_reg is
generic (a : positive := 14);
port(
	load : in std_logic; -- enable register to load data
	clk : in std_logic; -- system clock
	reset_bar : in std_logic; -- active low asynchronous reset
	d : in std_logic_vector(a-1 downto 0); -- data input
	q : out std_logic_vector(a-1 downto 0) -- register output
);
end frequency_reg;


architecture behavioral of frequency_reg is	
signal freq_value : std_logic_vector(a-1 downto 0);
begin

  -- process to increment the phase accumulator and output the frequency value
  process (clk, reset_bar)
    
  begin	
	  if (reset_bar = '0') then -- reset
        freq_value <= (others => '0');   
		
	  elsif (rising_edge(clk)) then
		  freq_value <= d; 
		 -- q<=d;
      --	if (load = '1') then -- load data into register
     --   freq_value := d;
      
     -- end if;
    end if;	 
	q <= std_logic_vector(freq_value);
  end process;
         
end behavioral;


--3
--Phase accumulator
library IEEE;
use IEEE.std_logic_1164.all; 
use IEEE.numeric_std.all; 
use work.all; 

entity phase_accumulator is
generic (
    a : positive := 14; -- width of phase accumulator
    m : positive := 7 -- width of phase accum output
);
port(
    clk : in std_logic; -- system clock
    reset_bar : in std_logic; -- asynchronous reset
    up : in std_logic; -- count direction control, 1 => up, 0 => dn
    d : in std_logic_vector(a-1 downto 0); -- count delta
    max : out std_logic := '0'; -- count has reached max value
    min : out std_logic := '0'; -- count has reached min value
    q : out std_logic_vector(m-1 downto 0) -- phase acc. output
);
end phase_accumulator;


architecture behavioral of phase_accumulator is
signal temp : std_logic_vector(13 downto 0);

begin
    process(clk, reset_bar)	
	variable value : integer := 0;  -- phase accumulator register
    begin
            if reset_bar = '0' then  -- asynchronous reset
                value := 0;  -- reset to all 0s
                max <= '0';  -- clear max output
                min <= '0';  -- clear min output 	
				
			elsif rising_edge(clk) then  -- on each positive clock edge
				
                
               	if up ='1' and (to_integer(unsigned(d)) + value >= ((2**a-1))) then  -- check for max
                    max <= '1';
					value := value;  
					min <= '0';		 
				elsif up = '1' and (value <= ((2**a) - 1)) then  -- increment
                    value := (value + to_integer(unsigned(d)));
					min <= '0';
                elsif (up = '0') and (to_integer(unsigned(d)) >= value) then  -- decrement
					value := 0;
					min <= '1';
					max <= '0';
                elsif (up = '0') and (value >= to_integer(unsigned(d))) then  -- check for min
					value := (value - to_integer(unsigned(d)));
					max <= '0';
				end if;	  
				
            end if;
            temp <= std_logic_vector(to_unsigned(value, a));  -- output lower bits of accumulator
    end process;
	q <= temp(13 downto 7);
end behavioral;


--4
--Phase accumulator Moore FSM
library ieee;
use ieee.std_logic_1164.all;
use work.all; 

entity phase_accumulator_fsm is
port(
    clk : in std_logic;        -- system clock
    reset_bar : in std_logic;  -- asynchronous reset
    max : in std_logic;        -- max count
    min : in std_logic;        -- min count
    up : out std_logic;        -- count direction
    pos : out std_logic        -- positive half of sine cycle
);
end phase_accumulator_fsm;

architecture Behavioral of phase_accumulator_fsm is

-- Define the states
type state_type is (up_pos, down_pos, up_neg, down_neg);
signal presentS : state_type; 
signal nextS : state_type;

begin

-- State register process
process(clk, reset_bar)
begin
    if (reset_bar = '0') then
        presentS <= up_pos; -- Reset to startial state
    elsif rising_edge(clk) then
        presentS <= nextS; -- Update state based on next state logic
    end if;
end process;

-- Next state and output logic process for up signal
process(presentS, min, max)
begin
    case presentS is
        when up_pos =>
            if (max = '1') then -- Switch to counting down
                nextS <= down_pos;
            else
                nextS <= up_pos;
            end if;
            
        when down_pos =>
            if (min = '1') then -- Switch back to counting up
                nextS <= up_neg;
            else
                nextS <= down_pos;
            end if;
            
        when up_neg =>
            if (max = '1') then -- Continue counting down
                nextS <= down_neg;
            else
                nextS <= up_neg;
            end if;
            
        when down_neg =>
            if (min = '1') then -- Continue counting up
                nextS <= up_pos;
            else
                nextS <= down_neg;
            end if;
    end case;
    end process;
    -- Output logic for up signal
	process(presentS)
	begin
    case presentS is

        when up_pos =>
		up <= '1'; -- Count up
        pos <= '1'; -- Still on positive half cycle		
        when down_pos =>
		up <= '0'; -- Count down
		pos <= '1'; -- Still on positive half cycle
        when up_neg =>
		up <= '1'; -- Don't count yet
		pos <= '0'; -- Switch to negative half cycle
        when down_neg =>
		up <= '0'; -- Count up
		pos <= '0'; -- Switch back to positive half cycle
    end case;
end process;

end Behavioral;

--5
--Sine table using table lookup 

library IEEE; 
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all; 
use work.all;

entity sine_table is
port(

addr : in std_logic_vector(6 downto 0);-- table address
sine_val : out std_logic_vector(6 downto 0)-- table entry value
);
end sine_table;

--}} End of automatically maintained section

architecture behavioral of sine_table is
    type sine_array is array (0 to 127) of std_logic_vector(6 downto 0); -- sine values for 0 to 360 degrees
    constant sine_values : sine_array := (
"0000000", "0000001", "0000011", "0000100", "0000110", "0000111", "0001001", "0001010",
"0001100", "0001110", "0001111", "0010001", "0010010", "0010100", "0010101", "0010111",
"0011000", "0011010", "0011100", "0011101", "0011111", "0100000", "0100010", "0100011",
"0100101", "0100110", "0101000", "0101001", "0101011", "0101100", "0101110", "0101111",
"0110000", "0110010", "0110011", "0110101", "0110110", "0111000", "0111001", "0111010",
"0111100", "0111101", "0111111", "1000000", "1000001", "1000011", "1000100", "1000101",
"1000111", "1001000", "1001001", "1001010", "1001100", "1001101", "1001110", "1001111",
"1010001", "1010010", "1010011", "1010100", "1010101", "1010111", "1011000", "1011001",
"1011010", "1011011", "1011100", "1011101", "1011110", "1011111", "1100000", "1100001",
"1100010", "1100011", "1100100", "1100101", "1100110", "1100111", "1101000", "1101001",
"1101010", "1101011", "1101100", "1101100", "1101101", "1101110", "1101111", "1110000",
"1110000", "1110001", "1110010", "1110011", "1110011", "1110100", "1110101", "1110101",
"1110110", "1110110", "1110111", "1110111", "1111000", "1111001", "1111001", "1111010",
"1111010", "1111010", "1111011", "1111011", "1111100", "1111100", "1111100", "1111101",
"1111101", "1111101", "1111110", "1111110", "1111110", "1111110", "1111111", "1111111",
"1111111", "1111111", "1111111", "1111111", "1111111", "1111111", "1111111", "1111111"
 --with value closest to 1 as 3F  
		
    );

begin 	
	
    sine_val <= sine_values(to_integer(unsigned(addr))); -- output the corresponding sine value for the given address  
end behavioral;


--5
--Adder or Subtractor 
Library IEEE; 
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all; 
use work.all; 

entity adder_subtractor is
port(
pos : in std_logic;-- indicates pos. or neg. half of cycle
sine_value : in std_logic_vector(6 downto 0);-- from sine table
dac_sine_val : out std_logic_vector(7 downto 0)-- output to DAC
);
end adder_subtractor;

--}} End of automatically maintained section

architecture behavioral of adder_subtractor is	
begin	
process (pos, sine_value)
 	variable value : unsigned(7 downto 0); 
begin
	if pos = '1' then 
		value := to_unsigned(to_integer(unsigned(sine_value)) + 128, 8); --positive cycle  
	else 
		value := to_unsigned(128 - to_integer(unsigned(sine_value)), 8);  --negative cycle
	end if;   
	--value(0) := not value(0);	
	dac_sine_val <=  std_logic_vector(value); 
end process;
end behavioral;


--7
--Structural approach for dds with frequency selection
library IEEE; 
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all; 
use work.all; 


entity dds_w_freq_select is
generic (a : positive := 14; m : positive := 7);
port(
clk : in std_logic;-- system clock
reset_bar : in std_logic;-- asynchronous reset
freq_val : in std_logic_vector(a - 1 downto 0);-- selects frequency
load_freq : in std_logic;-- pulse to load a new frequency selection
dac_sine_value : out std_logic_vector(7 downto 0);-- output to DAC
pos_sine : out std_logic-- positive half of sine wave cycle
);
attribute loc : string;
attribute loc of clk       : signal is "C13";
attribute loc of reset_bar : signal is "A13";
attribute loc of freq_val  : signal is "F8,C12,E10,F9,E8,E7,D7,C5,E6,A10,D9,B6,B5,B4";
attribute loc of load_freq : signal is "A3";
attribute loc of dac_sine_value  : signal is " N3,M1,L1,L5,J1,J2,H3,H1";
attribute loc of pos_sine  : signal is "F5";


end dds_w_freq_select;


architecture structural of dds_w_freq_select is
    -- internal signals
signal pos_or_neg : std_logic; 
signal sine_vals : std_logic_vector(6 downto 0);
signal q_sig : std_logic_vector(m-1 downto 0);
signal freq_reg_out : std_logic_vector(a-1 downto 0);
signal sigedge : std_logic;
signal upsig : std_logic;
signal maxsig, minsig : std_logic;

begin

    -- instantiate component entities
    U1 : entity edge_det
  port map(
    rst_bar => reset_bar,
    clk => clk,
    sig => load_freq,
    pos => '1',              -- positive edge detection
    sig_edge => sigedge
  );

  U2 : entity frequency_reg
  port map(
    load => sigedge,
    clk => clk,
    reset_bar => reset_bar,
    d => freq_val,
    q => freq_reg_out
  );

  U3 : entity phase_accumulator
  port map(
    clk => clk,
    reset_bar => reset_bar,
	up => upsig,
	max => maxsig,
	min => minsig,
	d => freq_reg_out,
	q => q_sig
  );
	
  U4 : entity phase_accumulator_fsm
  port map(
  clk => clk,
  reset_bar => reset_bar,
  max => maxsig,
  min => minsig, 
  up => upsig,
  pos => pos_or_neg
  );

  U5 : entity sine_table
  port map(
  addr => q_sig, 
  sine_val => sine_vals
  );

  U6 : entity adder_subtractor
  port map(
  sine_value => sine_vals,
  pos => pos_or_neg,
  dac_sine_val => dac_sine_value 
  );

  pos_sine <= pos_or_neg;

end structural;
