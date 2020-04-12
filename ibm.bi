Const hasfpu=1 ' siempre a 1 si tiene FPU (486DX)
const IDE_GRABA=1 ' para pruebas=1 graba, PELIGRO!!!!!!

static shared As Integer CPUID ' tipo de CPU, en los intel anteriores al 486DX2-66, es siempre 0. Solo cambia en los DX2-75
static shared As Integer cpu_multi ' multiplicador de CPU para el BUS de tipo DX2  (486dx2-66)
static shared As Integer CPUCLOCK ' velocidad REAL (66mhz=66666666, o sea, 66millones666mil666)
static shared As Integer cpu_exec ' para la rutina EXEC386() , seria REALSPEED/100

static shared As Single bus_timing

static shared As Integer timing_rr
static shared As Integer timing_mr, timing_mrl
static shared As Integer timing_rm, timing_rml
static shared As Integer timing_mm, timing_mml
static shared As Integer timing_bt, timing_bnt

' Program Counter
static shared As ULong pc
static shared As ULong oxpc ' copia de "pc" para algunas INS

' RAM, VRAM, ROM, VROM
static shared As UByte Ptr ram,vram
static shared As UByte Ptr rom,vrom
'static shared As UByte Ptr rambios

static shared As UByte isram(0 To 255) ' creo que vale con 256, por que son 256/16=16megas --> este sobra &h10000)
static shared As ULong rammask
static shared As Long mmu_perm=4

' cache
static shared As Long readlookup(0 To 255),readlookupp(0 To 255)
static shared As Long readlnext
static shared As Long writelookup(0 To 255),writelookupp(0 To 255)
static shared As Long writelnext
static shared As long cachelookup(0 To 255)
static shared As long cachelnext

' punteros a cache
static shared As ULong Ptr readlookup2
static shared As ULong Ptr writelookup2
static shared As Byte  Ptr cachelookup2
static shared As UByte Ptr pccache2



' PUNTEROS
static shared As ULong ptr eal_r
static shared As ULong ptr eal_w

' tabla de registros
static shared As UShort Ptr mod1add(0 To 1, 0 To 7)
static shared As ULong  Ptr mod1seg(0 To 7)
' variable usada en la tabla de registros "mod1add"
static shared As Integer slowrm(0 To 7)
' variable cero para la tabla de registros "mod1add"
static shared As UShort zero=0


static shared As Integer shadowbios ,shadowbios_write, grabar_bios, modo_bios
static shared As ULong pccache
static shared As UShort flags,eflags
static shared As Ulong oldds',olddslimit,olddslimitw
static shared As Ulong oldss,oldsslimit,oldsslimitw
static shared As Integer cpl_override  


static shared As Integer readflash ' no se aun que es, pero se usa mucho en el IDE

' variables puertos
static shared puertosb(0 To &hFFFF) as UByte
static shared puertosw(0 To &hFFFF) as UShort
static shared puertosl(0 To &hFFFF) as ULong

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
static shared As x86reg regs(0 To 7)

type x86seg
        As ULong base0
        As ULong limit
        As ULong limitw
        As UByte access0
        As UShort seg
End Type
static shared As x86seg gdt,ldt,idt,tr
static shared As x86seg _cs,_ds,_es,_ss,_fs,_gs
static shared As x86seg _oldds


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
static shared As crm CR1
#Define cr0 CR1.l
#Define msw CR1.w
static shared As Ulong cr2,cr3



Const C_FLAG  =&h0001
Const P_FLAG  =&h0004
Const A_FLAG  =&h0010
Const Z_FLAG  =&h0040
Const N_FLAG  =&h0080
Const T_FLAG  =&h0100
Const I_FLAG  =&h0200
Const D_FLAG  =&h0400
Const V_FLAG  =&h0800
Const NT_FLAG =&h4000
Const VM_FLAG =&h0002 'In EFLAGS
Const WP_FLAG =&h10000 'In CR0

#Define CPL ((_cs.access0 Shr 5) And 3)
#define IOPL ((flags Shr 12) And 3)
#define IOPLp ((modoprotegido=0) Or (CPL<=IOPL))
'
#Define DPL  ((segdat (2) Shr 13) And 3)
#Define DPL2 ((segdat2(2) Shr 13) And 3)
#Define DPL3 ((segdat3(2) Shr 13) And 3)


/'Timer'/
type PIT0
        Dim as ULong l(0 To 2)
        Dim as Double c(0 To 2)
        Dim as UByte m(0 To 2)
        Dim as UByte ctrl,ctrls(0 To 1)
        Dim as integer wp,rm(0 To 2),wm(0 To 2)
        Dim as UShort rl(0 To 2)
        Dim as Integer thit(0 To 2)
        Dim as Integer delay(0 To 2)
        Dim as Integer rereadlatch(0 To 2)
End Type
static shared As PIT0 pit

/'DMA'/
type DMA0
        Dim as UShort ab(0 To 3)
        Dim As UShort ac(0 To 3)
        Dim as UShort cb(0 To 3)
        Dim as Integer cc(0 To 3)
        Dim as Integer wp
        Dim as UByte m
        Dim As UByte mode(0 To 3)
        Dim as UByte page(0 To 3)
        Dim as UByte stat
        Dim as UByte command0
End Type
static shared As DMA0 dma,dma16

/'PPI'/
type PPI0
        Dim As Integer s2
        Dim As UByte pa,pb
End Type
static shared As PPI0 ppi





' modulo PIC i8259 (control de interrupciones IRQ)
static shared As Integer pic_intpending

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
static shared As PIC0 pic,pic2 



' contadores 
static shared As Single rtctime
static shared As Single vidtime
static shared As Single VGACONST1
static shared As Single VGACONST2
static shared As Single RTCCONST

/'Keyboard'/
static shared As Integer keybsenddelay


'''''''''''''''''''




'''' variables modulo x86
'
static shared As ULong easeg,eaaddr
static shared As ULong ealimit,ealimitw
static shared As UShort ea_rseg

Const JMP =1
Const CALL0 =2
Const IRET =3
Const INT0 =4

' estas dos variables apuntan con un define a la misma variable, y ambas afectan su contenido
static shared As Ulong rmdat32
#Define fetchdat rmdat32
#Define rmdat rmdat32

static shared As UShort oldcs
static shared As ULong oldpc
static shared As ULong oldcpl
static shared As Integer tempc
static shared As Integer cycles
static shared As Integer ssegs
static shared As Integer firstrepcycle
static shared As Integer rm,reg,modo
static shared As Integer inhlt
static shared As UByte opcode
static shared As Integer ins
static shared As Integer noint
static shared As Integer inint
static shared As UShort lastcs,lastpc
static shared As UByte znptable8(0 To &hFF)
static shared As UShort znptable16(0 To &hFFFF)
static shared As integer use32
static shared As ULong op32
static shared As Integer stack32
static shared As Integer optype
static shared As Integer cgate16,cgate32
static shared As Integer stimes= 0
static shared As Integer dtimes= 0
static shared As Integer btimes= 0
static shared As Integer intgatesize=16 ' por defecto, lo pongo en 16, por que sino, puede dar error en "x86_doabrt"
static shared As Integer gpf
static shared As Integer abrt
static shared As ULong abrt_error
static shared As Byte opcode2



' flags Z, N y P
static shared As ULong flags_zn
static shared As ubyte flags_p
function FLAG_N() As UByte
	return (flags_zn Shr 31)
End Function
function FLAG_Z() As UByte
	Return (flags_zn)
End Function
function FLAG_P() As ubyte
	Return (znptable8(flags_p) And P_FLAG)
End Function


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
Declare Sub resetx86(reset_cpu As integer) 
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
	Return Not(a)
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
