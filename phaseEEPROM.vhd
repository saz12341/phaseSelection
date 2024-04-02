library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;

library mylib;
use mylib.defAT93C46DController.all;
use mylib.defPhaseEEPROM.all;
use mylib.defPhaseSelection.all;

entity phaseEEPROM is
  generic(
    -- DEBUG --
    enDEBUG         : boolean := false
  );
  port ( 
    clk             : in  std_logic;  -- base clock
    rst             : in  std_logic;
    
    -- stored phase information ------------------------------------------------
    writeEnableCenter : in  std_logic;
    writeDataCenter   : in  std_logic_vector(kWidthShift-1 downto 0);
    writeBusyCenter   : out std_logic;
    writeEnableLength : in  std_logic;
    writeDataLength   : in  std_logic_vector(kWidthShift-1 downto 0);
    writeBusyLength   : out std_logic;
    
    readValidCenter   : out std_logic;
    readDataCenter    : out std_logic_vector(kWidthShift-1 downto 0);
    readValidLength   : out std_logic;
    readDataLength    : out std_logic_vector(kWidthShift-1 downto 0);
    
    -- AT93C460 ----------------------------------------------------------------
    CS              : out std_logic;  -- Chip Select
    SK              : out std_logic;  -- Serial Data Clock
    DI              : out std_logic;  -- Serial Data Input  (master-->slave)
    DO              : in  std_logic   -- Serial Data Output (slave-->master)
  );
end phaseEEPROM;

architecture Behavioral of phaseEEPROM is
  -- output
  signal output_busy_center   : std_logic := '0';
  signal output_busy_length   : std_logic := '0';
  
  signal output_valid_center  : std_logic := '0';
  signal output_data_center   : std_logic_vector(kWidthShift-1 downto 0);
  signal output_valid_length  : std_logic := '0';
  signal output_data_length   : std_logic_vector(kWidthShift-1 downto 0);
  
  -- input
  signal input_write_center   : std_logic;
  signal input_data_center    : std_logic_vector(kWidthShift-1 downto 0);
  
  signal input_write_length   : std_logic;
  signal input_data_length    : std_logic_vector(kWidthShift-1 downto 0);
  
  -- EWEN
  signal en_ewen      : std_logic_vector(kNumWords-1 downto 0);
  signal reg_en_ewen  : std_logic_vector(kNumWords-1 downto 0)  := (others=>'0');
  signal reduced_en_ewen  : std_logic;
  signal is_ewen          : std_logic;
  -- EWDS
  signal en_ewds      : std_logic_vector(kNumWords-1 downto 0);
  signal reg_en_ewds  : std_logic_vector(kNumWords-1 downto 0)  := (others=>'0');
  signal reduced_en_ewds  : std_logic;
  signal is_ewds          : std_logic;
  -- WRITE
  signal en_write     : std_logic_vector(kNumWords-1 downto 0);
  signal wdata_bus    : DataArray;
  signal reg_en_write : std_logic_vector(kNumWords-1 downto 0)  := (others=>'0');
  signal reg_wdata_bus: DataArray;
  signal is_write     : std_logic_vector(kNumWords-1 downto 0);
  -- READ
  signal en_read      : std_logic_vector(kNumWords-1 downto 0);
  signal reg_en_read  : std_logic_vector(kNumWords-1 downto 0)  := (others=>'0');
  signal is_read      : std_logic_vector(kNumWords-1 downto 0);
  signal reg_is_read  : std_logic_vector(kNumWords-1 downto 0)  := (others=>'0');
  signal rdata_bus    : DataArray;
    
  -- operate
  signal operate_en     : std_logic_vector((kNumWords*2+2)-1 downto 0);
  signal operate_is     : std_logic_vector((kNumWords*2+2)-1 downto 0);
  signal operate_ptr    : std_logic_vector(kNumWords-1 downto 0);
  signal operate_rdata  : std_logic_vector(kWidthData-1 downto 0);
  
  -- eeprom
  signal eeprom_ins   : instructionType   := idle;
  signal eeprom_busy  : std_logic;
  signal eeprom_addr  : std_logic_vector(kWidthAddr-1 downto 0);
  signal eeprom_din   : std_logic_vector(kWidthData-1 downto 0);
  signal eeprom_dout  : std_logic_vector(kWidthData-1 downto 0);
  
  attribute mark_debug : boolean;
  attribute mark_debug of eeprom_ins    : signal is enDEBUG;
  attribute mark_debug of eeprom_busy   : signal is enDEBUG;
  attribute mark_debug of eeprom_addr   : signal is enDEBUG;
  attribute mark_debug of eeprom_din    : signal is enDEBUG;
  attribute mark_debug of eeprom_dout   : signal is enDEBUG;
begin
  -- output port
  writeBusyCenter <= output_busy_center;
  writeBusyLength <= output_busy_length;
  
  readValidCenter <= output_valid_center;
  readDataCenter  <= output_data_center;
  readValidLength <= output_valid_length;
  readDataLength  <= output_data_length;
  
  -- input register ----------------------------------------------------------
  -- write acceptable_range_center
  u_write_center_process : process(clk, rst)
  begin
    if(rst = '1') then
      input_write_center  <= '0';
      output_busy_center  <= '0';
    elsif(clk'event and clk = '1') then
      if(output_busy_center='0' and writeEnableCenter='1') then
        input_write_center  <= '1';
        input_data_center   <= writeDataCenter;
        output_busy_center  <= '1';
      elsif(unsigned(reg_en_ewen(kAddrShiftCenter+kWordsCenter-1  downto kAddrShiftCenter))=0 and
            unsigned(reg_en_ewds(kAddrShiftCenter+kWordsCenter-1  downto kAddrShiftCenter))=0 and
            unsigned(reg_en_write(kAddrShiftCenter+kWordsCenter-1 downto kAddrShiftCenter))=0 and
            output_busy_center='1' and input_write_center='0' ) then
        output_busy_center  <= '0';
      else
        input_write_center  <= '0';
      end if;
    end if;
  end process u_write_center_process;
  
  gen_write_center: for i in 0 to kWordsCenter-1 generate
    en_ewen(i+kAddrShiftCenter)   <= input_write_center;
    en_ewds(i+kAddrShiftCenter)   <= input_write_center;
    en_write(i+kAddrShiftCenter)  <= input_write_center;
    wdata_bus(i+kAddrShiftCenter) <= input_data_center(kWidthData*(i+1)-1 downto kWidthData*i);
  end generate;
  
  -- write acceptable_range_length
  u_write_length_process : process(clk, rst)
  begin
    if(rst = '1') then
      input_write_length  <= '0';
      output_busy_length  <= '0';
    elsif(clk'event and clk = '1') then
      if(output_busy_length='0' and writeEnableLength='1') then
        input_write_length  <= '1';
        input_data_length   <= writeDataLength;
        output_busy_length  <= '1';
      elsif(unsigned(reg_en_ewen(kAddrShiftLength+kWordsLength-1  downto kAddrShiftLength))=0 and
            unsigned(reg_en_ewds(kAddrShiftLength+kWordsLength-1  downto kAddrShiftLength))=0 and
            unsigned(reg_en_write(kAddrShiftLength+kWordsLength-1 downto kAddrShiftLength))=0 and
            output_busy_length='1' and input_write_length='0' ) then
        output_busy_length  <= '0';
      else
        input_write_length  <= '0';
      end if;
    end if;
  end process u_write_length_process;
  
  gen_write_length: for i in 0 to kWordsLength-1 generate
    en_ewen(i+kAddrShiftLength)   <= input_write_length;
    en_ewds(i+kAddrShiftLength)   <= input_write_length;
    en_write(i+kAddrShiftLength)  <= input_write_length;
    wdata_bus(i+kAddrShiftLength) <= input_data_length(kWidthData*(i+1)-1 downto kWidthData*i);
  end generate;
  
  -- output register ---------------------------------------------------------
  -- read acceptable_range_center
  output_valid_center <= '1' when unsigned(not reg_is_read(kAddrShiftCenter+kWordsCenter-1 downto kAddrShiftCenter))=0 else '0';
  gen_read_center: for i in 0 to kWordsCenter-1 generate
    output_data_center(kWidthData*(i+1)-1 downto kWidthData*i)  <= rdata_bus(i+kAddrShiftCenter);
  end generate;
  
  -- read acceptable_range_length
  output_valid_length <= '1' when unsigned(not reg_is_read(kAddrShiftLength+kWordsLength-1 downto kAddrShiftLength))=0 else '0';
  gen_read_length: for i in 0 to kWordsLength-1 generate
    output_data_length(kWidthData*(i+1)-1 downto kWidthData*i)  <= rdata_bus(i+kAddrShiftLength);
  end generate;
  
  -- auto read output --------------------------------------------------------
  gen_auto_read: for i in 0 to kNumWords-1 generate
  u_auto_read_process : process(clk,rst)
  begin
    if(rst = '1') then
      en_read(i)  <= '0';
    elsif(clk'event and clk = '1') then
      if(reg_en_read(i)='0' and (is_write(i)='1' or reg_is_read(i)='0'))then -- eeprom write new data / output register doesn't have data
        en_read(i)  <= '1';
      else
        en_read(i)  <= '0';
      end if;
    end if;
  end process u_auto_read_process;
  end generate;
  
  -- eeprom operate ----------------------------------------------------------
  -- EWEN
  gen_ewen: for i in 0 to kNumWords-1 generate
  u_ewen_process : process(clk, rst)
  begin
    if(rst = '1') then
      reg_en_ewen(i) <= '0';
    elsif(clk'event and clk = '1') then
      if(en_ewen(i)='1') then
        reg_en_ewen(i) <= '1';
      elsif(is_ewen='1') then
        reg_en_ewen(i) <= '0';
      end if;
    end if;
  end process u_ewen_process;
  end generate;
  reduced_en_ewen <= '1' when unsigned(reg_en_ewen)/=0 else '0';
  
  -- EWDS
  gen_ends: for i in 0 to kNumWords-1 generate
  u_ewds_process : process(clk, rst)
  begin
    if(rst = '1') then
      reg_en_ewds(i) <= '0';
    elsif(clk'event and clk = '1') then
      if(en_ewds(i)='1') then
        reg_en_ewds(i) <= '1';
      elsif(is_ewds='1') then
        reg_en_ewds(i) <= '0';
      end if;
    end if;
  end process u_ewds_process;
  end generate;
  reduced_en_ewds <= '1' when unsigned(reg_en_ewds)/=0 else '0';

  -- WRITE
  gen_write: for i in 0 to kNumWords-1 generate
  u_write_process : process(clk, rst)
  begin
    if(rst = '1') then
      reg_en_write(i) <= '0';
    elsif(clk'event and clk = '1') then
      if(en_write(i)='1') then
        reg_en_write(i)   <= '1';
        reg_wdata_bus(i)  <= wdata_bus(i);
      elsif(is_write(i)='1') then
        reg_en_write(i)   <= '0';
      end if;
    end if;
  end process u_write_process;
  end generate;
  
  -- READ
  gen_read: for i in 0 to kNumWords-1 generate
  u_read_process : process(clk, rst)
  begin
    if(rst = '1') then
      reg_en_read(i) <= '0';
    elsif(clk'event and clk = '1') then
      if(en_read(i)='1') then
        reg_en_read(i) <= '1';
      elsif(is_read(i)='1') then
        reg_en_read(i) <= '0';
      end if;
    end if;
  end process u_read_process;
  u_read_output_process : process(clk, rst)
  begin
    if(rst = '1') then
      reg_is_read(i)  <= '0';
    elsif(clk'event and clk = '1') then
      if(is_read(i)='1') then
        reg_is_read(i)  <= '1';
        rdata_bus(i)    <= operate_rdata;
      elsif(en_write(i)='1') then
        reg_is_read(i)  <= '0';
      end if;
    end if;
  end process u_read_output_process;
  end generate;
  
  -- operate
  -- operate intput
  operate_en  <= reg_en_read & reduced_en_ewds & reg_en_write & reduced_en_ewen;
  -- operate output
  is_ewen     <= operate_is(0);
  is_write    <= operate_is(kNumWords downto 1);
  is_ewds     <= operate_is(kNumWords+1);
  is_read     <= operate_is(kNumWords*2+1 downto kNumWords+2);
  -- eeprom addr
  gen_eeprom_addr: for i in 0 to kNumWords-1 generate
    eeprom_addr <= std_logic_vector(to_unsigned(i,eeprom_addr'length)) when operate_ptr(i)='1' else (others=>'Z');
  end generate;
  -- eeprom din
  gen_eeprom_din: for i in 0 to kNumWords-1 generate
    eeprom_din <= reg_wdata_bus(i) when operate_ptr(i)='1' else (others=>'Z');
  end generate;

  u_operate_run_process : process(clk, rst)
    variable buf_eeprom_busy  : std_logic;
    variable wait_is          : std_logic := '0';
    variable operate_run      : std_logic_vector((kNumWords*2+2)-1 downto 0)  := (others=>'0');
  begin
    if(rst = '1') then
      buf_eeprom_busy := '0';
      wait_is         := '0';
      operate_run     := (others=>'0');
      operate_is      <= (others=>'0');
    elsif(clk'event and clk = '1') then
      if(unsigned(operate_run)=0) then
        operate_run   := operate_en and std_logic_vector(unsigned(not operate_en) + 1);
        operate_ptr   <= operate_run(kNumWords downto 1) or operate_run(kNumWords*2+1 downto kNumWords+2);
        
        -- eeprom_ins
        if(operate_run(0)/='0')then
          eeprom_ins  <= EWEN;
        elsif(operate_run(kNumWords+1)/='0')then
          eeprom_ins  <= EWDS;
        elsif(unsigned(operate_run(kNumWords downto 1))/=0)then
          eeprom_ins  <= WRITE;
        elsif(unsigned(operate_run(kNumWords*2+1 downto kNumWords+2))/=0)then
          eeprom_ins  <= READ;
        else
          eeprom_ins  <= idle;
        end if;
        
        operate_is  <= (others=>'0');
        
      elsif(eeprom_busy='0' and buf_eeprom_busy='1') then
        -- eeprom_ins
        operate_is    <= operate_run;
        operate_rdata <= eeprom_dout;
        wait_is       := '1';
      
      elsif(wait_is='1' and unsigned(operate_is and operate_en)=0) then
        operate_is    <= (others=>'0');
        operate_run   := (others=>'0');
        wait_is       := '0';
      
      else
        eeprom_ins    <= idle;
        operate_is  <= (others=>'0');
      end if;

      buf_eeprom_busy := eeprom_busy;
    end if;
  end process u_operate_run_process;
  
  -- eeprom
  u_AT93C46DController : entity mylib.AT93C46DController
  generic map(
    enDEBUG => enDEBUG
  )
  port map(
    clk         => clk,
    rst         => rst,
    
    instruction =>eeprom_ins,
    busy        =>eeprom_busy,
    addr        =>eeprom_addr,
    dataIn      =>eeprom_din,
    dataOut     =>eeprom_dout,
    
    CS          =>CS,
    SK          =>SK,
    DI          =>DI,
    DO          =>DO
  );
  
end Behavioral;
