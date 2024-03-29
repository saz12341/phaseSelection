library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;

library mylib;
use mylib.defAT93C46DController.all;

entity AT93C46DController is
  generic(
    -- DEBUG --
    enDEBUG         : boolean := false
  );
  port ( 
    clk             : in  std_logic;      -- clock
    rst             : in  std_logic;      -- clock reset
    
    -- instruction -------------------------------------------------------------
    instruction     : in  InstructionType;  -- instruction
    busy            : out std_logic;        -- busy
    addr            : in  std_logic_vector(kWidthAddr-1 downto 0);  -- address
    dataIn          : in  std_logic_vector(kWidthData-1 downto 0);  -- data in
    dataOut         : out std_logic_vector(kWidthData-1 downto 0);  -- data out

    -- AT93C460 ----------------------------------------------------------------
    CS              : out std_logic;  -- Chip Select
    SK              : out std_logic;  -- Serial Data Clock
    DI              : out std_logic;  -- Serial Data Input  (master-->slave)
    DO              : in  std_logic   -- Serial Data Output (slave-->master)
  );
end AT93C46DController;

architecture Behavioral of AT93C46DController is
  -- instruction
  signal ins_instruction  : instructionType;
  signal ins_addr         : std_logic_vector(kWidthAddr-1 downto 0);
  signal ins_data_in      : std_logic_vector(kWidthData-1 downto 0);
  
  signal ins_busy         : std_logic := '0';
  signal ins_data_out     : std_logic_vector(kWidthData-1 downto 0);
  
  -- AT93C460
  signal spi_cs   : std_logic := '0';
  signal spi_sk   : std_logic := '0';
  signal spi_di   : std_logic := '0';
  
  -- instruction
  signal status   : spiStatusType := idle;
  signal status_instruction : instructionType;
  signal ptr_di   : integer range 0 to kMaxWidthDi-1;
  signal reg_di   : std_logic_vector(kMaxWidthDi-1 downto 0);
  signal ptr_do   : integer range 0 to kMaxWidthDo-1;
  signal reg_do   : std_logic_vector(kMaxWidthDo-1 downto 0);
  
  attribute mark_debug : boolean;
  attribute mark_debug of spi_cs    : signal is enDEBUG;
  attribute mark_debug of spi_sk    : signal is enDEBUG;
  attribute mark_debug of spi_di    : signal is enDEBUG;
  attribute mark_debug of ptr_di    : signal is enDEBUG;
  attribute mark_debug of reg_di    : signal is enDEBUG;
  attribute mark_debug of ptr_do    : signal is enDEBUG;
  attribute mark_debug of reg_do    : signal is enDEBUG;
begin
  -- instruction
  busy    <= ins_busy;
  dataOut <= ins_data_out;
  
  -- AT93C460
  CS  <= spi_cs;   
  SK  <= spi_sk;   
  DI  <= spi_di;

  -- spi_sk
  u_spi_sk_process : process(clk, rst)
    variable count  : integer range 0 to kClockDivider-1  := 0;
  begin
    if(rst = '1') then
      count   := 0;
      spi_sk  <= '0';
    elsif(clk'event and clk = '1') then
      if(count=kClockDivider-1)then
        count := 0;
      else
        count := count+1;
      end if;
      if(count<kClockDivider/2-1)then
          spi_sk  <='0';
      else
          spi_sk  <='1';
      end if;
    end if;
  end process u_spi_sk_process;

  -- spi_cs
  spi_cs  <= '1' when status=tx else
             '1' when status=tr else
             '1' when status=waitReady0 else
             '1' when status=waitReady1 else '0';
  
  -- spi_di
  spi_di  <= reg_di(ptr_di) when status=tx else '0';
  
  -- instruction
  u_instruction_process : process(clk, rst)
  begin
    if(rst = '1') then
      ins_instruction <= idle;
    elsif(clk'event and clk = '1') then
      if(status=idle and instruction/=idle)then
        ins_instruction <= instruction;
        ins_addr        <= addr;
        ins_data_in     <= dataIn;
      elsif(status=waitTcs and ins_instruction/=idle)then
        ins_instruction <= idle;
        ins_data_out    <= reg_do(kWidthData-1 downto 0);
      end if;
    end if;
  end process u_instruction_process;

  ins_busy  <= '0' when status=idle else '1';

  -- spi_process
  u_spi_process : process(spi_sk, rst)
  begin
    if(rst = '1') then
      status    <= idle;
    elsif(spi_sk'event and spi_sk = '0') then
      case status is
        when idle       =>
          case ins_instruction is
            when EWEN   =>
              status              <= tx;
              status_instruction  <= EWEN;
              ptr_di              <= kWidthDiEWEN-1;
              reg_di(kWidthDiEWEN-1 downto 0) <= kDiEWEN; 
            when EWDS   =>
              status              <= tx;
              status_instruction  <= EWDS;
              ptr_di              <= kWidthDiEWDS-1;
              reg_di(kWidthDiEWDS-1 downto 0) <= kDiEWDS; 
            when READ   =>
              status              <= tx;
              status_instruction  <= READ;
              ptr_di              <= kWidthOpcode+kWidthAddr-1;
              reg_di(kWidthOpcode+kWidthAddr-1 downto kWidthAddr) <= kOpcodeREAD; 
              reg_di(kWidthAddr-1 downto 0)                       <= ins_addr; 
            when WRITE  =>
              status              <= tx;
              status_instruction  <= WRITE;
              ptr_di              <= kWidthOpcode+kWidthAddr+kWidthData-1;
              reg_di(kWidthOpcode+kWidthAddr+kWidthData-1 downto kWidthAddr+kWidthData) <= kOpcodeWRITE; 
              reg_di(kWidthAddr+kWidthData-1 downto kWidthData)                         <= ins_addr; 
              reg_di(kWidthData-1 downto 0)                                             <= ins_data_in; 
            when ERASE  =>
              status              <= tx;
              status_instruction  <= ERASE;
              ptr_di              <= kWidthOpcode+kWidthAddr-1;
              reg_di(kWidthOpcode+kWidthAddr-1 downto kWidthAddr) <= kOpcodeERASE; 
              reg_di(kWidthAddr-1 downto 0)                       <= ins_addr;
            when others  =>
              status          <= idle;
          end case;
        
        when tx         =>
          if(ptr_di=0)then
            if(status_instruction=READ)then
              status  <= tr;
            elsif(status_instruction=WRITE or status_instruction=ERASE)then
              status  <= waitWETcs;
            else
              status  <= waitTcs;
            end if;
          else
            ptr_di<=ptr_di-1;
          end if;
        
        when tr         =>
          if(ptr_do=0)then
            status  <= waitTcs;
          end if;
        
        when waitTcs    =>
          status  <= idle;
        
        when waitWETcs  =>
          status  <= waitReady0;
        
        when waitReady0  =>
          if(DO='0')then
            status  <= waitReady1;
          end if;
        
        when waitReady1  =>
          if(DO='1')then
            status  <= waitTcs;
          end if;
        
        when others      =>
          status  <= idle;
      end case;
    end if;
  end process u_spi_process;
  
  u_spi_do_process : process(spi_sk, rst)
  begin
    if(rst = '1') then
      ptr_do  <= kMaxWidthDo-1;
    elsif(spi_sk'event and spi_sk = '0') then
      if(status_instruction=READ and (status=tx or status=tr))then
        if( (ptr_do=kMaxWidthDo-1 and DO='0') or (ptr_do/=kMaxWidthDo-1 and ptr_do/=0) )then
          ptr_do          <= ptr_do-1;
        end if;
        reg_do(ptr_do)  <= DO;
      else
        ptr_do  <= kMaxWidthDo-1;
      end if;
    end if;
  end process u_spi_do_process;
  
end Behavioral;
