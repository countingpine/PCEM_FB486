#Define is486 1 ' siempre a 1 , si "ademas" de 386, es 486
#Define hasfpu 1 ' siempre a 1 si tiene FPU (486DX)
#Define IDE_GRABA 1 ' para pruebas=1 graba, PELIGRO!!!!!!

Dim Shared as Integer CPUID ' tipo de CPU, en los intel anteriores al 486DX2-66, es siempre 0. Solo cambia en los DX2-75
Dim Shared As Integer cpu_multi ' multiplicador de CPU para el BUS de tipo DX2  (486dx2-66)
Dim Shared As Integer CPUCLOCK ' velocidad REAL (66mhz=66666666, o sea, 66millones666mil666)
Dim Shared As Integer cpu_exec ' para la rutina EXEC386() , seria REALSPEED/100

Dim Shared As Single bus_timing

Dim Shared as Integer timing_rr
Dim Shared as Integer timing_mr, timing_mrl
Dim Shared as Integer timing_rm, timing_rml
Dim Shared as Integer timing_mm, timing_mml
Dim Shared as Integer timing_bt, timing_bnt

' Program Counter
Dim Shared As ULong pc
Dim Shared as ULong oxpc ' copia de "pc" para algunas INS

' RAM, VRAM, ROM, VROM
Dim Shared as UByte Ptr ram,vram
Dim Shared As UByte Ptr rom,vrom
'Dim Shared As UByte Ptr rambios

Dim Shared as UByte isram(256) ' creo que vale con 256, por que son 256/16=16megas --> este sobra &h10000)
Dim Shared as ULong rammask
Dim Shared as Long mmu_perm=4

' cache
Dim Shared as Long readlookup(256),readlookupp(256)
Dim Shared as Long readlnext
Dim Shared as Long writelookup(256),writelookupp(256)
Dim Shared as Long writelnext
Dim Shared as long cachelookup(256)
Dim Shared as long cachelnext

' punteros a cache
Dim Shared as ULong Ptr readlookup2
Dim Shared as ULong Ptr writelookup2
Dim Shared as Byte  Ptr cachelookup2
Dim Shared as UByte Ptr pccache2



' PUNTEROS
Dim Shared as ULong ptr eal_r
Dim Shared as ULong ptr eal_w

' tabla de registros
Dim Shared as UShort Ptr mod1add(2, 8)
Dim Shared as ULong  Ptr mod1seg(8)
' variable usada en la tabla de registros "mod1add"
Dim Shared as Integer slowrm(8)
' variable cero para la tabla de registros "mod1add"
Dim Shared as UShort zero=0


Dim Shared as Integer shadowbios ,shadowbios_write, grabar_bios, modo_bios
Dim Shared as ULong pccache
Dim Shared as UShort flags,eflags
Dim Shared As Ulong oldds',olddslimit,olddslimitw
Dim Shared As Ulong oldss,oldsslimit,oldsslimitw
Dim Shared as Integer cpl_override  


Dim Shared As Integer readflash ' no se aun que es, pero se usa mucho en el IDE

' variables puertos
Dim Shared puertosb(65535) as UByte
Dim Shared puertosw(65535) as UShort
Dim Shared puertosl(65535) as ULong

#Define EAX regs(0).l
#Define ECX regs(1).l
#Define EDX regs(2).l
#Define EBX regs(3).l
#Define ESP regs(4).l
#Define EBP regs(5).l
#Define ESI regs(6).l
#Define EDI regs(7).l

#Define AX regs(0).w
#Define CX regs(1).w
#Define DX regs(2).w
#Define BX regs(3).w
#Define SP regs(4).w
#Define BP regs(5).w
#Define SI regs(6).w
#Define DI regs(7).w

#Define AL regs(0).lb
#Define AH regs(0).hb
#Define CL regs(1).lb
#Define CH regs(1).hb
#Define DL regs(2).lb
#Define DH regs(2).hb
#Define BL regs(3).lb
#Define BH regs(3).hb


' estructura 32 bits 
' si hacemos EAX=&h12345678 sale AL=78, AH=56, y EAX=5678
' si hacemos AL=00, sale EAX=12345600, AH=56, EAX=5600, etc
Type x86reg
 Union
    As Ulong l ' guarda EAX
    As UShort w ' guarda AX
    Type
       As UByte lb ' guarda AL, estos dos en conjunto, generan AX
       As UByte hb ' guarda AH
	 End type
 End Union
End Type 
Dim Shared As x86reg regs(8)

type x86seg
        As ULong base0
        As ULong limit
        As ULong limitw
        As UByte access0
        As UShort seg
End Type
Dim Shared as x86seg gdt,ldt,idt,tr
Dim Shared as x86seg _cs,_ds,_es,_ss,_fs,_gs
Dim Shared as x86seg _oldds


' Segments
'  _cs,_ds,_es,_ss are the segment structures
'  CS,DS,ES,SS is the 16-bit data
'  cs,ds,es,ss are defines to the bases 
'  (si no me equivoco, son los "reales", por ejemplo: cs0 es CS1*16 (FFFF*16=FFFF0)
#Define CS1 _cs.seg
#Define DS1 _ds.seg
#Define ES1 _es.seg
#Define SS1 _ss.seg
#Define FS1 _fs.seg
#Define GS1 _gs.seg
'
#Define cs0 _cs.base0
#Define ds0 _ds.base0
#Define es0 _es.base0
#Define ss0 _ss.base0
#Define fs0 _fs.base0
#Define gs0 _gs.base0


Type crm
	Union
        As ULong l
        As ushort w
   End Union 
End Type
Dim Shared As crm CR1
#Define cr0 CR1.l
#Define msw CR1.w
Dim Shared as Ulong cr2,cr3



#Define C_FLAG  &h0001
#define P_FLAG  &h0004
#define A_FLAG  &h0010
#define Z_FLAG  &h0040
#define N_FLAG  &h0080
#define T_FLAG  &h0100
#define I_FLAG  &h0200
#define D_FLAG  &h0400
#define V_FLAG  &h0800
#define NT_FLAG &h4000
#define VM_FLAG &h0002 'In EFLAGS
#Define WP_FLAG &h10000 'In CR0

#Define CPL ((_cs.access0 Shr 5) And 3)
#define IOPL ((flags Shr 12) And 3)
#define IOPLp ((modoprotegido=0) Or (CPL<=IOPL))
'
#Define DPL  ((segdat (2) Shr 13) And 3)
#Define DPL2 ((segdat2(2) Shr 13) And 3)
#Define DPL3 ((segdat3(2) Shr 13) And 3)


/'Timer'/
type PIT0
        Dim as ULong l(3)
        Dim as Double c(3)
        Dim as UByte m(3)
        Dim as UByte ctrl,ctrls(2)
        Dim as integer wp,rm(3),wm(3)
        Dim as UShort rl(3)
        Dim as Integer thit(3)
        Dim as Integer delay(3)
        Dim as Integer rereadlatch(3)
End Type
Dim Shared as PIT0 pit

/'DMA'/
type DMA0
        Dim as UShort ab(4)
        Dim As UShort ac(4)
        Dim as UShort cb(4)
        Dim as Integer cc(4)
        Dim as Integer wp
        Dim as UByte m
        Dim As UByte mode(4)
        Dim as UByte page(4)
        Dim as UByte stat
        Dim as UByte command0
End Type
Dim Shared As DMA0 dma,dma16

/'PPI'/
type PPI0
        Dim As Integer s2
        Dim As UByte pa,pb
End Type
Dim Shared As PPI0 ppi





' modulo PIC i8259 (control de interrupciones IRQ)
Dim Shared as Integer pic_intpending

Declare Sub pic_init()  
Declare Sub pic2_init()  
Declare Sub pic_reset() 

Declare Sub picint(num As UShort ) 
Declare Sub picintc(num As UShort )

/'PIC'/
Type PIC0
        Dim As UByte icw1,mask,ins,pend,mask2
        Dim As Integer icw
        Dim As UByte vector
        Dim As Integer read0
End Type
Dim Shared As PIC0 pic,pic2 



' contadores 
Dim Shared as Single rtctime
Dim Shared as Single vidtime
Dim Shared as Single VGACONST1
Dim Shared as Single VGACONST2
Dim Shared as Single RTCCONST

/'Keyboard'/
Dim Shared as Integer keybsenddelay


'''''''''''''''''''




'''' variables modulo x86
'
Dim Shared as ULong easeg,eaaddr
Dim Shared As ULong ealimit,ealimitw
Dim Shared as UShort ea_rseg

#Define JMP 1
#Define CALL0 2
#Define IRET 3
#Define INT0 4


' flags Z, N y P
Dim Shared as ULong flags_zn
Dim Shared as ubyte flags_p
#Define FLAG_N (flags_zn Shr 31)
#Define FLAG_Z (flags_zn)
#define FLAG_P (znptable8(flags_p) And P_FLAG)


' estas dos variables apuntan con un define a la misma variable, y ambas afectan su contenido
Dim Shared as Ulong rmdat32
#Define fetchdat rmdat32
#Define rmdat rmdat32

Dim Shared as UShort oldcs
Dim Shared as ULong oldpc
Dim Shared as ULong oldcpl
Dim Shared as Integer tempc
Dim Shared as Integer cycles
Dim Shared as Integer ssegs
Dim Shared as Integer firstrepcycle
Dim Shared as Integer rm,reg,modo
Dim Shared as Integer inhlt
Dim Shared as UByte opcode
Dim Shared as Integer ins
Dim Shared as Integer noint
Dim Shared as Integer inint
Dim Shared as UShort lastcs,lastpc
Dim Shared as UByte znptable8(256)
Dim Shared as UShort znptable16(65536)
Dim Shared as integer use32
Dim Shared As ULong op32
Dim Shared as Integer stack32
Dim Shared as Integer optype
Dim Shared as Integer cgate16,cgate32
Dim Shared as Integer stimes= 0
Dim Shared as Integer dtimes= 0
Dim Shared as Integer btimes= 0
Dim Shared as Integer intgatesize=16 ' por defecto, lo pongo en 16, por que sino, puede dar error en "x86_doabrt"
Dim Shared as Integer gpf
Dim Shared as Integer abrt
Dim Shared as ULong abrt_error
Dim Shared as Byte opcode2


' posibles errores en memoria (variable ABRT)
Enum
        ABRT_NONE = 0,
        ABRT_GEN  = 1,
        ABRT_TS  = &hA,
        ABRT_NP  = &hB,
        ABRT_SS  = &hC,
        ABRT_GPF = &hD,
        ABRT_PF  = &hE
End Enum


''''''''''''''''''''''''''''''''''''''''''''''''''''''
' DECLARACIONES OBLIGATORIAS
''''''''''''''''''''''''''''''''''''''''''''''''''''''

' en modulo X86.BAS
Declare Sub resetx86(i486 As integer) '1=486, 0=386
Declare Sub softresetx86()
Declare Sub makemod1table()
Declare Sub makeznptable()

' en modulo 386A.BAS
Declare Function getword() As ushort 
Declare Function getlong() As ULong 

' en modulo X86SEG.BAS
Declare Sub taskswitch286(seg As UShort , segdat() As UShort , is32 As Integer )  
Declare Sub x86_doabrt(ByVal x86_abrt As Integer ) 
Declare Sub pmodeint(num As Integer , soft As Integer )  

' en modulo i8253_PIT.bas
Declare Sub readdma0()

' en IO.BAS 
declare Sub loadCMOS(bios As string)



Function inv(a As uLongint) As uLongint
	Return -1-1*(a)
End Function

Function modoprotegido() As UShort
	Return IIf(msw And 1,1,0)
End Function


' VIDEO
Declare sub svga_init()
Declare Sub svga_poll()  
Declare Sub svga_recalctimings()  
Declare Function svga_read(addr As Ulong ) As UByte   
Declare Sub svga_write(addr As ULong , valor As UByte )  
Declare Sub svga_doblit(y1 As Integer , y2 As Integer )
Declare Sub svga_out(addr As UShort , valor As UByte )   
Declare Function svga_in(addr As UShort ) As UByte  
