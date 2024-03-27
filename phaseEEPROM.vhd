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
    
    -- store phase -------------------------------------------------------------
    writeEnableMean   : in  std_logic;
    writeDataMean     : in  std_logic_vector(kWidthShift-1 downto 0);
    writeBusyMean     : out std_logic;
    writeEnableWidth  : in  std_logic;
    writeDataWidth    : in  std_logic_vector(kWidthShift-1 downto 0);
    writeBusyWidth    : out std_logic;
    
    readEnableMean    : in  std_logic;
    readDataMean      : out std_logic_vector(kWidthShift-1 downto 0);
    readValidMean     : out std_logic;
    readEnableWidth   : in  std_logic;
    readDataWidth     : out std_logic_vector(kWidthShift-1 downto 0);
    readValidWidth    : out std_logic;
    
    -- AT93C460 ----------------------------------------------------------------
    CS              : out std_logic;  -- Chip Select
    SK              : out std_logic;  -- Serial Data Clock
    DI              : out std_logic;  -- Serial Data Input  (master-->slave)
    DO              : in  std_logic   -- Serial Data Output (slave-->master)
  );
end phaseEEPROM;

architecture Behavioral of phaseEEPROM is
  
  signal reg_write_mean   : std_logic := '0';
  signal reg_wdata_mean   : std_logic_vector(kWidthShift-1 downto 0);
  signal reg_write_width  : std_logic := '0';
  signal reg_wdata_width  : std_logic_vector(kWidthShift-1 downto 0);
  
  signal reg_read_mean    : std_logic := '0';
  signal reg_rdata_mean   : std_logic_vector(kWidthShift-1 downto 0);
  signal reg_valid_mean   : std_logic := '0';
  signal reg_read_width   : std_logic := '0';
  signal reg_rdata_width  : std_logic_vector(kWidthShift-1 downto 0);
  signal reg_valid_width  : std_logic := '0';
  
  signal status_operate   : operateType       := idle;
  signal status_eeprom    : eepromStatusType  := idle;
  signal reg_read_data    : std_logic_vector(kWidthShift-1 downto 0);
  signal reg_write_data   : std_logic_vector(kWidthShift-1 downto 0);
  signal reg_status_eeprom: eepromStatusType  := idle;
  
  signal eeprom_ins       : InstructionType := idle;
  signal eeprom_busy      : std_logic       := '0';
  signal eeprom_addr      : std_logic_vector(kWidthAddr-1 downto 0);
  signal eeprom_din       : std_logic_vector(kWidthData-1 downto 0);
  signal eeprom_dout      : std_logic_vector(kWidthData-1 downto 0);
  
  signal reg_eeprom_busy    : std_logic     := '0';
  signal eeprom_complete    : std_logic     := '0';
  
  attribute mark_debug : boolean;
  attribute mark_debug of eeprom_ins    : signal is enDEBUG;
  attribute mark_debug of eeprom_busy   : signal is enDEBUG;
  attribute mark_debug of eeprom_addr   : signal is enDEBUG;
  attribute mark_debug of eeprom_din    : signal is enDEBUG;
  attribute mark_debug of eeprom_dout   : signal is enDEBUG;
begin
  -- output register
  writeBusyMean   <= reg_write_mean;
  writeBusyWidth  <= reg_write_width;
  readDataMean    <= reg_rdata_mean;
  readValidMean   <= reg_valid_mean;
  readDataWidth   <= reg_rdata_width;
  readValidWidth  <= reg_valid_width;
  
  -- write mean
  u_write_mean_process : process(clk, rst)
  begin
    if(rst = '1') then
      reg_write_mean  <= '0';
    elsif(clk'event and clk = '1') then
      if(reg_write_mean='0' and writeEnableMean='1')then
        reg_write_mean  <= '1';
        reg_wdata_mean  <= writeDataMean;
      elsif(status_operate=writeMean and status_eeprom=complete)then
        reg_write_mean  <= '0';
      end if;
    end if;
  end process u_write_mean_process;

  -- write width
  u_write_width_process : process(clk, rst)
  begin
    if(rst = '1') then
      reg_write_width <= '0';
    elsif(clk'event and clk = '1') then
      if(reg_write_width='0' and writeEnableWidth='1')then
        reg_write_width <= '1';
        reg_wdata_width <= writeDataWidth;
      elsif(status_operate=writeWidth and status_eeprom=complete)then
        reg_write_width <= '0';
      end if;
    end if;
  end process u_write_width_process;
  
  -- read mean
  u_read_mean_process : process(clk, rst)
  begin
    if(rst = '1') then
      reg_read_mean  <= '0';
    elsif(clk'event and clk = '1') then
      if(readEnableMean='1')then
        reg_read_mean  <= '1';
      elsif(status_operate=readMean and status_eeprom=complete)then
        reg_read_mean  <= '0';
      end if;
    end if;
  end process u_read_mean_process;

  reg_valid_mean  <= '0' when readEnableMean='1' else
                     '1' when (status_operate=readMean and status_eeprom=complete) else reg_valid_mean;
  reg_rdata_mean  <= reg_read_data when (status_operate=readMean and status_eeprom=complete) else reg_rdata_mean;
  
  -- read width
  u_read_width_process : process(clk, rst)
  begin
    if(rst = '1') then
      reg_read_width  <= '0';
    elsif(clk'event and clk = '1') then
      if(readEnableWidth='1')then
        reg_read_width  <= '1';
      elsif(status_operate=readWidth and status_eeprom=complete)then
        reg_read_width  <= '0';
      end if;
    end if;
  end process u_read_width_process;

  reg_valid_width <= '0' when readEnableWidth='1' else
                     '1' when (status_operate=readWidth and status_eeprom=complete) else reg_valid_width;
  reg_rdata_width <= reg_read_data when (status_operate=readWidth and status_eeprom=complete) else reg_rdata_width;
  
  -- operate
  u_operate_process : process(clk, rst)
  begin
    if(rst = '1') then
      status_operate  <= idle;
      status_eeprom   <= idle;
    elsif(clk'event and clk = '1') then
      case status_eeprom is
        when idle       =>
          if(reg_write_mean='1')then
            status_operate  <= writeMean;
            status_eeprom   <= ewen;
          elsif(reg_write_width='1')then
            status_operate  <= writeWidth;
            status_eeprom   <= ewen;
          elsif(reg_read_mean='1')then
            status_operate  <= readMean;
            status_eeprom   <= data1;
          elsif(reg_read_width='1')then
            status_operate  <= readWidth;
            status_eeprom   <= data1;
          else
            status_operate  <= idle;
            status_eeprom   <= idle;
          end if;
        when data1      =>
          if(eeprom_complete='1')then
            status_eeprom <= data2;
          end if;
        when data2      =>
          if(eeprom_complete='1')then
            status_eeprom <= data3;
          end if;
        when data3      =>
          if(eeprom_complete='1')then
            status_eeprom <= data4;
          end if;
        when data4      =>
          if(eeprom_complete='1')then
            if(status_operate=writeMean or status_operate=writeWidth)then
              status_eeprom <= ewds;
            else
              status_eeprom <= complete;
            end if;
          end if;
        when ewen       =>
          if(eeprom_complete='1')then
            status_eeprom <= data1;
          end if;
        when ewds       =>
          if(eeprom_complete='1')then
            status_eeprom <= complete;
          end if;
        when complete   =>
          status_operate  <= idle;
          status_eeprom   <= idle;
        when others     =>
          status_operate  <= idle;
          status_eeprom   <= idle;
      end case;
    end if;
  end process u_operate_process;

  -- status eeprom 
  u_status_eeprom_process : process(clk)
  begin
    if(clk'event and clk = '1') then
      reg_status_eeprom <= status_eeprom; 
    end if;
  end process u_status_eeprom_process;
  
  -- eeprom instruction
  u_eeprom_ins_process : process(clk, rst)
  begin
    if(rst = '1') then
      eeprom_ins  <= idle;
    elsif(clk'event and clk = '1') then
      if(status_eeprom/=reg_status_eeprom)then
        case status_eeprom is
          when data1  =>
            if(status_operate=writeMean or status_operate=writeWidth)then
              eeprom_ins  <= WRITE;
            else
              eeprom_ins  <= READ;
            end if;
          when data2  =>
            if(status_operate=writeMean or status_operate=writeWidth)then
              eeprom_ins  <= WRITE;
            else
              eeprom_ins  <= READ;
            end if;
          when data3  =>
            if(status_operate=writeMean or status_operate=writeWidth)then
              eeprom_ins  <= WRITE;
            else
              eeprom_ins  <= READ;
            end if;
          when data4  =>
            if(status_operate=writeMean or status_operate=writeWidth)then
              eeprom_ins  <= WRITE;
            else
              eeprom_ins  <= READ;
            end if;
          when ewen   =>
            eeprom_ins  <= EWEN;
          when ewds   =>
            eeprom_ins  <= EWDS;
          when others =>
            eeprom_ins  <= idle;
        end case;
      else
        eeprom_ins  <= idle;
      end if;
    end if;
  end process u_eeprom_ins_process;
  
  -- eeprom addr
  u_eeprom_addr_process : process(clk)
  begin
    if(clk'event and clk = '1') then
      if(status_operate=writeMean or status_operate=readMean)then
        if(status_eeprom=data1)then
          eeprom_addr <= kAddrMean1;
        elsif(status_eeprom=data2)then
          eeprom_addr <= kAddrMean2;
        elsif(status_eeprom=data3)then
          eeprom_addr <= kAddrMean3;
        elsif(status_eeprom=data4)then
          eeprom_addr <= kAddrMean4;
        end if;
      elsif(status_operate=writeWidth or status_operate=readWidth)then
        if(status_eeprom=data1)then
          eeprom_addr <= kAddrWidth1;
        elsif(status_eeprom=data2)then
          eeprom_addr <= kAddrWidth2;
        elsif(status_eeprom=data3)then
          eeprom_addr <= kAddrWidth3;
        elsif(status_eeprom=data4)then
          eeprom_addr <= kAddrWidth4;
        end if;
      end if;
    end if;
  end process u_eeprom_addr_process;
  
  -- eeprom din
  reg_write_data  <= reg_wdata_mean   when (status_operate=writeMean)   else
                     reg_wdata_width  when (status_operate=writeWidth)  else
                     reg_write_data;
  u_eeprom_din_process : process(clk)
  begin
    if(clk'event and clk = '1') then
      if(status_eeprom=data1)then
        eeprom_din <= reg_write_data(kWidthData-1 downto 0);
      elsif(status_eeprom=data2)then
        eeprom_din <= reg_write_data(kWidthData*2-1 downto kWidthData);
      elsif(status_eeprom=data3)then
        eeprom_din <= reg_write_data(kWidthData*3-1 downto kWidthData*2);
      elsif(status_eeprom=data4)then
        eeprom_din <= reg_write_data(kWidthData*4-1 downto kWidthData*3);
      end if;
    end if;
  end process u_eeprom_din_process;

  -- eeprom dout
  u_eeprom_dout1_process : process(clk)
  begin
    if(clk'event and clk = '1') then
      if(status_eeprom=data1 and eeprom_complete='1' and (status_operate=readMean or status_operate=readWidth))then
        reg_read_data(kWidthData-1 downto 0)  <= eeprom_dout;
      end if;
    end if;
  end process u_eeprom_dout1_process;

  u_eeprom_dout2_process : process(clk)
  begin
    if(clk'event and clk = '1') then
      if(status_eeprom=data2 and eeprom_complete='1' and (status_operate=readMean or status_operate=readWidth))then
        reg_read_data(kWidthData*2-1 downto kWidthData)  <= eeprom_dout;
      end if;
    end if;
  end process u_eeprom_dout2_process;

  u_eeprom_dout3_process : process(clk)
  begin
    if(clk'event and clk = '1') then
      if(status_eeprom=data3 and eeprom_complete='1' and (status_operate=readMean or status_operate=readWidth))then
        reg_read_data(kWidthData*3-1 downto kWidthData*2)  <= eeprom_dout;
      end if;
    end if;
  end process u_eeprom_dout3_process;

  u_eeprom_dout4_process : process(clk)
  begin
    if(clk'event and clk = '1') then
      if(status_eeprom=data4 and eeprom_complete='1' and (status_operate=readMean or status_operate=readWidth))then
        reg_read_data(kWidthData*4-1 downto kWidthData*3)  <= eeprom_dout;
      end if;
    end if;
  end process u_eeprom_dout4_process;

  -- eeprom busy
  u_eeprom_busy_process : process(clk)
  begin
    if(clk'event and clk = '1') then
      reg_eeprom_busy <= eeprom_busy;
      if(reg_eeprom_busy='1' and eeprom_busy='0')then
        eeprom_complete <= '1';
      else
        eeprom_complete <= '0';
      end if;
    end if;
  end process u_eeprom_busy_process;
  
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
