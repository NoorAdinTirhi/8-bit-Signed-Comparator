------------------------------------------------------
--****************Library of Gates*******************
------------------------------------------------------
------------------------------------------------------
--******************NOT******************************
------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
entity inverter is
	port(I : in std_logic;
	O : out std_logic);
end;
architecture simple of inverter is
begin
	O <= not I after 2ns;
end;
------------------------------------------------------
--******************NAND******************************
------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
entity NAND2 is
	port(I0,I1 : in std_logic;
	O : out std_logic);
end;
architecture simple of NAND2 is
begin
	O <= I0 nand I1 after 5ns;
end;
------------------------------------------------------
--******************NOR******************************
------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
entity NOR2 is
	port(I0,I1 : in std_logic;
	O : out std_logic);
end;
architecture simple of NOR2 is
begin
	O <= I0 nor I1 after 5ns;
end;
------------------------------------------------------
--******************AND******************************
------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
entity AND2 is
	port(I0,I1 : in std_logic;
	O : out std_logic);
end;
architecture simple of AND2 is
begin
	O <= I0 and I1 after 7ns;
end;
------------------------------------------------------
--**************Generic AND**************************
------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
entity ANDN is
	generic ( n : positive := 1);
	port(I : in std_logic_vector ( n-1 downto 0);
	O : out std_logic);
end;
architecture simple of ANDN is
signal temp : std_logic_vector (n - 1 downto 0);
begin
	temp(0) <= I(0);
	O <= temp(n-1) after 7ns;
	g1 : for a in 0 to n-2 generate
		temp(a + 1) <= temp(a) and I(a);
	end generate;
end;
------------------------------------------------------
--********************OR******************************
------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
entity OR2 is
	port(I0,I1 : in std_logic;
	O : out std_logic);
end;
architecture simple of OR2 is
begin
	O <= I0 or I1 after 7ns;
end;
------------------------------------------------------
--**************Generic OR**************************
------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
entity ORN is
	generic ( n : positive := 4);
	port(I : in std_logic_vector ( n-1 downto 0);
	O : out std_logic);
end;
architecture simple of ORN is
signal temp : std_logic := '1';
begin
	g1 : for a in 0 to n-1 generate
		temp <= temp or I(a);
	end generate;
	O <=  temp;
end;
------------------------------------------------------
--******************XNOR******************************
------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
entity XNOR2 is
	port(I0,I1 : in std_logic;
	O : out std_logic);
end;
architecture simple of XNOR2 is
begin
	O <= I0 xnor I1 after 9ns;
end;
------------------------------------------------------
--******************XOR*******************************
------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
entity XOR2 is
	port(I0,I1 : in std_logic;
	O : out std_logic);
end;
architecture simple of XOR2 is
begin
	O <= I0 xor I1 after 12ns;
end;
------------------------------------------------------
--******************3bit NAND************************
------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
entity NAND3 is
	port (I0,I1,I2 : in std_logic;
	O : out std_logic);
end;
architecture simple of NAND3 is
signal temp : std_logic;
begin
	temp <= I1 and I0 and I2;
	O <= not temp after 5ns;
end;
------------------------------------------------------
--******************3bit AND*************************
------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
entity AND3 is
	port (I0,I1,I2 : in std_logic;
	O : out std_logic);
end;
architecture simple of AND3 is
signal temp : std_logic;
begin
	temp <= I1 and I0 and I2;
	O <= temp after 7ns;
end;
----------------------------------------------------
--******************7bit NOR************************
------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
entity NOR7 is
	port (I : in std_logic_vector (6 downto 0);
	O : out std_logic);
end;
architecture simple of NOR7 is
signal temp : std_logic;
begin
	temp <= I(6) or I(5) or I(4) or I(3) or I(2) or I(1) or I(0);
	O <= not temp after 5ns;
end;
------------------------------------------------------
--****************************************************
------------------------------------------------------ 
------------------------------------------------------
--******************Full Adder***********************
------------------------------------------------------
-- producing Full Adder by inverted gates to reduve propagation delay
library ieee;
use ieee.std_logic_1164.all;
entity partialadder is
	port (A,B,Cin : in std_logic;
	Sum,Cout : out std_logic);
end;
architecture simple of partialadder is
signal Sin,S,Sumin,C0,C1 : std_logic;
begin
	g0 : entity work.XNOR2(simple) port map (A,B,Sin);
	g5 : entity work.inverter(simple) port map (Sin,S);
	g1 : entity work.XNOR2(simple) port map (S,Cin,Sumin);
	g6 : entity work.inverter(simple) port map (Sumin,Sum);
	g2 : entity work.NAND2(simple) port map (A,B,C0);
	g3 : entity work.NAND2(simple) port map (S,Cin,C1);
	g4 : entity work.NAND2(simple) port map (C0,C1,Cout);
end;
------------------------------------------------------This is used to and minterms instead 
--******************2-level NAND********************* using AND gates into OR gates.
------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
entity LVL2NAND is
	port (A,B,C : in std_logic;
	O : out std_logic);
end;
architecture simple of LVL2NAND is
Signal L,M,N : std_logic;
begin
	g0 : entity work.NAND2(simple) port map (A,B,L);
	g1 : entity work.NAND2(simple) port map (A,C,M);
	g2 : entity work.NAND2(simple) port map (C,B,N);
	g3 : entity work.NAND3(simple) port map (L,M,N,O);
end;
------------------------------------------------------
--****************Signed Comparator*******************
------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
entity SComp is
	port (A,B : in std_logic_vector (7 downto 0);
	Grt,Less,Eq : out std_logic);
end;
------------------------------------------------------
--********8 Bit(7 Bit Adder) Signed Comparator********
------------------------------------------------------
-- using a series of full adders to produce the ripple adders
-- the carry out of each full adder will be taken as the carry in
-- of the next adjacent full adder.
library ieee;
use ieee.std_logic_1164.all;
entity adder7 is
	port (A,B : in std_logic_vector (6 downto	0);
	--Cin : in std_logic;"	no need for Cin, since it always be '1' since we are going to be using a subtractor
	Sum : out std_logic_vector (6 downto 0);
	Cout: out std_logic);
end;
-- this circuit turns the adder circuit into a subtractor
-- this is so we can deffrentiate between A and B,
-- the output of the adder doesn't tell us anything 
-- about the comparison between A and B.
architecture subtractor of adder7 is
signal carry : std_logic_vector(7 downto 0);
signal Bin : std_logic_vector(6 downto 0);
begin
	carry(0) <= '1';
	Cout <= carry(7);
	gen : for i in 0 to 6 generate		  
		n1 : entity work.inverter(simple) port map (B(i),Bin(i));
		e1 : entity work.partialadder(simple) port map (A(i),Bin(i),carry(i),Sum(i),carry(i+1));
	end generate;
end;
architecture adder of SComp is
Signal Sum : std_logic_vector(6 downto 0 );
Signal Cout,Coutin,Zero,notZero,AP,BP,AG,AG1,AG2,BG,BG1,BG2,x7 : std_logic;
-- The AG bits are used to propogate the minterms of A>B to the nand gate
-- The BG bits are used to propogate the minterms of A<B to the nand gate
-- x7 bit is A(7) = B(7) so we can check for equality
begin
	-- use the adder7 circuit to produce Cout and Sum
	G1 : entity work.adder7(subtractor) port map (A(6 downto 0),B(6 downto 0),Sum,Cout);
	-- use a 7 bit nor gate to produce the sum = 0  bit
	G2 : entity work.NOR7(simple) port map (Sum,Zero);
	-- invert the Cout,A(7),B(7) and the Zero bits
	NZ : entity work.inverter(simple) port map (Zero, notZero);
	NB : entity work.inverter(simple) port map(A(7), AP);
	NA : entity work.inverter(simple) port map(B(7), BP);
	NC : entity	work.inverter(simple) port map(Cout,Coutin);
	-- produce the A > B, using nand gates with the design accquired by the truth table
	A1 : entity work.NAND2(simple) port map	(AP,B(7),AG);
	A2 : entity work.NAND3(simple) port map (Cout,notZero,Ap,AG1);
	A3 : entity work.NAND3(simple) port map (Cout,notZero,B(7),AG2);
	A4 : entity work.NAND3(simple) port map (AG,AG1,AG2,Grt);
	-- produce the A < B, using nand gates with the design accquired by the truth table
	B1 : entity work.NAND2(simple) port map	(A(7),BP,BG);
	B2 : entity work.NAND3(simple) port map (Coutin,notZero,A(7),BG1);
	B3 : entity work.NAND3(simple) port map (Coutin,notZero,Bp,BG2);
	B4 : entity work.NAND3(simple) port map (BG,BG1,BG2,Less);
	-- produce the A = B, producing x7 with an xnor of A(7) B(7) and anding the produced x7 bit with the zero bit to get the A=B bits
	E1 : entity work.XNOR2(simple) port map (A(7),B(7),x7);
	E2 : entity work.AND2(simple) port map (x7,Zero,Eq);
		
end;
--*****************************************************************
--This architecture is the same as the working one, the only difference 
--is that the A>B and B>A is inverted, but this is also 2ns faster.
architecture flawedadder of SComp is
Signal Sum : std_logic_vector(6 downto 0 );
Signal Cout,Coutin,Zero,notZero,AP,BP,AG,AG1,AG2,BG,BG1,BG2,x7,EqT : std_logic;
begin
	G1 : entity work.adder7(subtractor) port map (A(6 downto 0),B(6 downto 0),Sum,Cout);
	G2 : entity work.NOR7(simple) port map (Sum,Zero);
	NZ : entity work.inverter(simple) port map (Zero, notZero);
	NB : entity work.inverter(simple) port map(A(7), AP);
	NA : entity work.inverter(simple) port map(B(7), BP);
	NC : entity	work.inverter(simple) port map(Cout,Coutin);
	A1 : entity work.NAND2(simple) port map	(AP,B(7),AG);
	A2 : entity work.NAND3(simple) port map (Cout,notZero,Ap,AG1);
	A3 : entity work.NAND3(simple) port map (Cout,notZero,B(7),AG2);
	-- intentional flaw using And3 instead of Nand3
	A4 : entity work.AND3(simple) port map (AG,AG1,AG2,Grt);
	B1 : entity work.NAND2(simple) port map	(A(7),BP,BG);
	B2 : entity work.NAND3(simple) port map (Coutin,notZero,A(7),BG1);
	B3 : entity work.NAND3(simple) port map (Coutin,notZero,Bp,BG2);
	-- intentional flaw using And3 instead of Nand3
	B4 : entity work.AND3(simple) port map (BG,BG1,BG2,Less);
	E1 : entity work.XNOR2(simple) port map (A(7),B(7),x7);
	-- intentional flaw using Nand2 instead of And2
	E2 : entity work.NAND2(simple) port map (x7,Zero,Eq);
end;
------------------------------------------------------
--8 Bit(7 Bit Magnitude Comparator) Signed Comparator
------------------------------------------------------
-- I attempted to create the 7 bit magnitude comparator structurally
-- but it didn't work.
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
entity MagnitudeComp is
	port (A,B : in std_logic_vector (6 downto 0);
	Grt,Equ,Less : out std_logic);
end;
architecture Simple of MagnitudeComp is
begin
Equ <= '1' when A = B ELSE '0' after 16ns;
Grt <= '1' when A > B ELSE '0' after 23ns;
Less <= '1' when A < B ELSE '0' after 23ns;
--signal Grtin,Lessin : std_logic;
--signal x,AG,BG,Ain,Bin,AGRT,BGRT : std_logic_vector (6 downto 0);
--signal BBus7, ABus7 : std_logic_vector (6 downto 0);
--signal BBus6, ABus6 : std_logic_vector (5 downto 0);
--signal BBus5, ABus5 : std_logic_vector (4 downto 0);
--signal BBus4, ABus4 : std_logic_vector (3 downto 0);
--signal BBus3, ABus3 : std_logic_vector (2 downto 0);
--signal BBus2, ABus2 : std_logic_vector (1 downto 0);
--begin
--	g1 : for i in 0 to 6 generate
--		X1 : entity work.XNOR2(simple) port map (A(i),B(i),x(i));
--		BG1 : entity work.inverter(simple) port map (A(i),Ain(i));
--		BG2 : entity work.AND2(simple) port map (Ain(i),B(i),BG(i)); -- BG(i)= A`(i).B(i) 
--		AG1 : entity work.inverter(simple) port map (B(i),Bin(i));
--		AG2 : entity work.AND2(simple) port map (Bin(i),A(i),AG(i)); -- AG(i)= A(i).B`(i) 
--	end generate;
--	--g2 : for i in 0 to 5 generate
----		A1 : entity work.ANDN(simple) generic map (7 - i) port map (ABus(
----	end generate;
--	AEquB : entity work.ANDN(simple) generic map (7) port map (x, Equ);
--	BBus7 <= BG(0) & X(6 downto 1);
--	BBus6 <= BG(1) & X(6 downto 2);
--	BBus5 <= BG(2) & X(6 downto 3);
--	BBus4 <= BG(3) & X(6 downto 4);
--	BBus3 <= BG(4) & X(6 downto 5);
--	BBus2 <= BG(5) & X(6 downto 6);
--	ABus7 <= AG(0) & X(6 downto 1);
--	ABus6 <= AG(1) & X(6 downto 2);
--	ABus5 <= AG(2) & X(6 downto 3);
--	ABus4 <= AG(3) & X(6 downto 4);
--	ABus3 <= AG(4) & X(6 downto 5);
--	ABus2 <= AG(5) & X(6 downto 6);
--	B0 : entity work.ANDN(simple) generic map (7) port map (BBus7,BGRT(0));
--	B1 : entity work.ANDN(simple) generic map (6) port map (BBus6,BGRT(1));
--	B2 : entity work.ANDN(simple) generic map (5) port map (BBus5,BGRT(2));
--	B3 : entity work.ANDN(simple) generic map (4) port map (BBus4,BGRT(3));
--	B4 : entity work.ANDN(simple) generic map (3) port map (BBus3,BGRT(4));
--	B5 : entity work.ANDN(simple) generic map (2) port map (BBus2,BGRT(5));
--	BGRT(6) <= BG(6);
--	A0 : entity work.ANDN(simple) generic map (7) port map (ABus7,AGRT(0));
--	A1 : entity work.ANDN(simple) generic map (6) port map (ABus6,AGRT(1));
--	A2 : entity work.ANDN(simple) generic map (5) port map (ABus5,AGRT(2));
--	A3 : entity work.ANDN(simple) generic map (4) port map (ABus4,AGRT(3));
--	A4 : entity work.ANDN(simple) generic map (3) port map (ABus3,AGRT(4));
--	A5 : entity work.ANDN(simple) generic map (2) port map (ABus2,AGRT(5));
--	AGRT(6) <= AG(6);
--	AGrtBin : entity work.NOR7(simple) port map (AGRT,Grtin);
--	AGrtB : entity work.inverter(simple) port map (Grtin,Grt);
--	BGrtAin : entity work.NOR7(simple) port map (BGRT,Lessin);
--	BGrtA : entity work.inverter(simple) port map (Lessin,Less);
end;
architecture MagComp of SComp  is
signal G,L,E : std_logic; -- these signals carry the results of the magnitude comparator
signal Ap,Bp : std_logic; -- these signals will carry the inverse of the most significat bit of each operand
signal x7 : std_logic; -- this signal will carry the equals bit between A(7) B(7)
begin
	MC : entity work.MagnitudeComp(simple) port map (A(6 downto 0),B(6 downto 0),G,E,L);
	E1 : entity work.XNOR2(simple) port map (A(7),B(7),x7);
	E2 : entity work.AND2 (simple) port map (x7,E,Eq);
	G1 : entity work.inverter (simple) port map (A(7) ,Ap);
	L1 : entity work.inverter (simple) port map (B(7) ,Bp);
	G2 : entity work.LVL2NAND(simple) port map (Ap,B(7),G,Grt);
	L2 : entity work.LVL2NAND(simple) port map (Bp,A(7),L,Less); 
end;
-- this the same circuit but with some wrong gates to make it flawed, specefically where it comes to X7
architecture FlawedMag of SComp  is
signal G,L,E : std_logic; -- these signals carry the results of the magnitude comparator
signal Ap,Bp : std_logic; -- these signals will carry the inverse of the most significat bit of each operand
signal x7 : std_logic; -- this signal will carry the equals bit between A(7) B(7)
begin
	MC : entity work.MagnitudeComp(simple) port map (A(6 downto 0),B(6 downto 0),G,E,L);
	E1 : entity work.NOR2(simple) port map (A(7),B(7),x7);
	E2 : entity work.NAND2 (simple) port map (x7,E,Eq);
	G1 : entity work.inverter (simple) port map (A(7) ,Ap);
	L1 : entity work.inverter (simple) port map (B(7) ,Bp);
	G2 : entity work.LVL2NAND(simple) port map (Ap,B(7),G,Grt);
	L2 : entity work.LVL2NAND(simple) port map (Bp,A(7),L,Less); 
end;
------------------------------------------------------
--*************Testing and Verification*************
------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_signed.all;
use ieee.std_logic_arith.all;
entity TestGenerator is
	port (clk : in std_logic;
	A,B : out std_logic_vector (7 downto 0);
	Grt,Equ,Less : out std_logic); 
end;
architecture timed of TestGenerator is
begin
	process
	begin
			for i in -128 to 127 loop
				for j in -128 to 127 loop
					A <= conv_std_logic_vector(i,8);
					B <= conv_std_logic_vector(j,8);
					if (i>j) then
						Grt <= '1';
						Equ <= '0';
						Less <= '0';
					elsif (i=j) then
						Grt <= '0';
						Equ <= '1';
						Less <= '0';
					elsif (i<j) then
						Grt <= '0';
						Equ <= '0';
						Less <= '1';
					end if;
					wait until rising_edge(clk);
				end loop;
			end loop;
			wait;
	end process;
end;

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_signed.all;
use ieee.std_logic_arith.all;
entity ResAnalyser is
	port (EGrt,EEqu,ELess,Grt,Equ,Less,clk : in  std_logic;
	Excp,Actu : out std_logic_vector (2 downto 0));
end;
architecture simple of ResAnalyser is
signal Excpt,Actut : std_logic_vector (2 downto 0);
begin
	Excp <= Excpt;
	Actu <= Actut;
	Excpt(2) <= EGrt;
	Excpt(1) <= EEqu;
	Excpt(0) <= ELess;
	Actut(2) <= Grt;
	Actut(1) <= Equ;
	Actut(0) <= Less;
	-- this is going to compare  the excpected and actual results every rising edge of the clock
	-- this is to make sure that the comparison is done after the Signed comparator produces the
	-- correct output
	process (clk)
	begin
		if (rising_edge(clk)) then
			Assert Excpt = Actut
			report "Comparator output is wrong"
			Severity Error;
		end if;
	end process;
end;
-- Test analysis for the working as excpected circuits
library ieee;
use ieee.std_logic_1164.all;
entity CompTest is
	port (clk : std_logic);
end;
architecture complete of CompTest is
signal Ai,Bi : std_logic_vector (7 downto 0);
signal ActRes,ExcRes : std_logic_vector (2 downto 0);
signal ReadExc,ReadActu : std_logic_vector ( 2 downto 0);
begin
	A1 : entity work.TestGenerator(timed) port map (clk,Ai,Bi,ExcRes(2),ExcRes(1),ExcRes(0));
	A2 : entity work.scomp(adder) port map (Ai,Bi,ActRes(2) ,ActRes(0), ActRes(1));
	A3 : entity work.ResAnalyser port map (ExcRes(2),ExcRes(1),ExcRes(0),ActRes(2),ActRes(1),ActRes(0),clk,ReadExc, ReadActu);
end;

architecture MagComp of CompTest is
signal Ai,Bi : std_logic_vector (7 downto 0);
signal ActRes,ExcRes : std_logic_vector (2 downto 0);
signal ReadExc,ReadActu : std_logic_vector ( 2 downto 0);
begin
	A1 : entity work.TestGenerator(timed) port map (clk,Ai,Bi,ExcRes(2),ExcRes(1),ExcRes(0));
	A2 : entity work.scomp(MagComp) port map (Ai,Bi,ActRes(2) ,ActRes(0), ActRes(1));
	A3 : entity work.ResAnalyser port map (ExcRes(2),ExcRes(1),ExcRes(0),ActRes(2),ActRes(1),ActRes(0),clk,ReadExc, ReadActu);
end;
-- Test analysis for the flawed circuits made above
architecture flawedadder of CompTest is
signal Ai,Bi : std_logic_vector (7 downto 0);
signal ActRes,ExcRes : std_logic_vector (2 downto 0);
signal ReadExc,ReadActu : std_logic_vector ( 2 downto 0);
begin
	A1 : entity work.TestGenerator(timed) port map (clk,Ai,Bi,ExcRes(2),ExcRes(1),ExcRes(0));
	A2 : entity work.scomp(flawedadder) port map (Ai,Bi,ActRes(2) ,ActRes(0), ActRes(1));
	A3 : entity work.ResAnalyser port map (ExcRes(2),ExcRes(1),ExcRes(0),ActRes(2),ActRes(1),ActRes(0),clk,ReadExc, ReadActu);
end;

architecture flawedMag of CompTest is
signal Ai,Bi : std_logic_vector (7 downto 0);
signal ActRes,ExcRes : std_logic_vector (2 downto 0);
signal ReadExc,ReadActu : std_logic_vector ( 2 downto 0);
begin
	A1 : entity work.TestGenerator(timed) port map (clk,Ai,Bi,ExcRes(2),ExcRes(1),ExcRes(0));
	A2 : entity work.scomp(flawedMag) port map (Ai,Bi,ActRes(2) ,ActRes(0), ActRes(1));
	A3 : entity work.ResAnalyser port map (ExcRes(2),ExcRes(1),ExcRes(0),ActRes(2),ActRes(1),ActRes(0),clk,ReadExc, ReadActu);
end;





