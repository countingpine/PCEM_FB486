Type colorRGB
    As ubyte r, g, b
End Type

Type RGB_EGA
        As ubyte r, g, b
End Type    
static shared As RGB_EGA vgapal(0 To 255)

' esta variable me la invento: es para meter las lineas dibujadas por el modulo SVGA
Dim Shared As ULong vga_buffer(0 to 2047, 0 to 2047)

' variable que desactiva la VGA a peticion de la SVGA ET4000
static shared As Integer vga_ports_activos


static shared As Integer changeframecount=2 
static shared As Integer svgaon=0
static shared As Ulong svgarbank
static shared As ULong svgawbank
static shared As UByte svgaseg ' ET4000
static shared As UByte svgaseg2 ' ET4000_W32
static shared As UByte cgastat 
static shared As UByte gdcreg(0 To 15)
static shared As Integer gdcaddr
static shared As UByte attrregs(0 To 31)
static shared As integer attraddr
static shared As Integer attrff=0
static shared As UByte rotatevga(0 To 7, 0 To 255)

static shared As UByte colourcompare,colournocare

static shared As UByte seqregs(0 To 31)
static shared As Integer seqaddr

static shared As ULong Ptr pallook
static shared As ULong pallook16(0 To 255),pallook64(0 To 255) ' estas paletas son para EGA solo
static shared As ULong pallook256(0 To 255) ' paleta VGA

static shared As Integer dacread,dacwrite,dacpos,dacmask,dacstatus
static shared As Integer fullchange ' si cambia algun dato en la SVGA, esta se actualiza
static shared As double dispontime,dispofftime,disptime
static shared As Integer vidclock ' guarda el registro de velocidad SVGA: 0=VGACONST1, sino VGACONST2
'
static shared As Integer bpp
static shared As ULong vrammask
static shared As UByte changedvram(0 To 8191)
static shared As Integer charseta,charsetb
static shared As UByte writemask,charset
static shared As Integer writemode,readmode,readplane,chain4
'
static shared As Integer egapal(0 To 15)
static shared As Integer displine
static shared As Integer scrblank=0
static shared As UByte edatlookup(0 To 3, 0 To 3)
static shared As Integer olddisplines,oldxsize
static shared As Integer wx,wy

static shared As ubyte crtc(0 To 127)
static shared As UByte crtcreg

' aqui se inicializan solo 18 registros, el resto, se ponen a FF en "resetvideo()"
static shared As ubyte crtcmask(0 To 127)={&hFF,&hFF,&hFF,&hFF,&h7F,&h1F,&h7F,&h7F,&hF3,&h1F,&h7F,&h1F,&h3F,&hFF,&h3F,&hFF,&hFF,&hFF}

static shared As Integer video_timing_b, video_timing_w, video_timing_l
static shared As Integer video_res_x, video_res_y, video_bpp
static shared As Integer mdacols(0 To 255, 0 To 1, 0 To 1)
static shared As ubyte charbuffer(0 To 255)
static shared As Integer oddeven=0

static shared As Double isa_timing

/'Vertical timings'/
static shared As Integer svga_vtotal, svga_dispend, svga_vsyncstart, svga_split
/'Horizontal timings'/
static shared As Integer svga_hdisp, svga_htotal, svga_rowoffset
/'Flags - svga_lowres = 1/2 clock in 256+ colour modes, svga_interlace = interlace mode enabled'/
static shared As Integer svga_lowres, svga_interlace
static shared As Integer svga_hdisp_on
static shared As Double svga_clock
static shared As uByte svga_miscout=0
static shared As Integer svga_vram_limit

' estaticas
static Shared as UByte dla,dlb,dlc,dld
static Shared as Integer vc,sc
Static Shared as Integer linepos
Static Shared as Integer scrollcache
static Shared as Integer con,cursoron,cgablink
Static Shared as Integer linecountff
Static Shared as Integer vslines
static Shared as UByte svga_rotate(0 To 7, 0 To 255)
Static Shared as Integer svga_dispon
Static Shared As Integer svga_fast ' selecciona el tipo de acceso a VRAM
Static Shared as Integer firstline_draw=2000, lastline_draw=0
Static Shared as Integer firstline=1000,lastline=0
Static Shared as Ulong ma,maback,ca,svga_ma ' direcciones VRAM

function makecol32(r As ubyte, g As ubyte, b As ubyte) As ULong
	Return (b or (g shl 8) or (r shl 16))
End Function

Sub video_updatetiming() 

		' tiempos segun su velocidad, por defecto es la 0
        '{VIDEO_ISA, 8, 16, 32}, speed=0
        '{VIDEO_ISA, 6,  8, 16},		=1
        '{VIDEO_ISA, 3,  3,  6},		=2
        '{VIDEO_BUS, 4,  8, 16},		=3
        '{VIDEO_BUS, 4,  5, 10},		=4
        '{VIDEO_BUS, 3,  3,  4} 		=5

		' los timing se calculan en la rutina "setpitclock" del modulo "i8253_pit.bas"
		
		' usando BUS ISA, isa_timing=8.33 (66mhz/8mhz)
        video_timing_b = (isa_timing * 3) 
        video_timing_w = (isa_timing * 3) 
        video_timing_l = (isa_timing * 6) 
        
      ' usando BUS estandar de la CPU, bus_timing=2 (66mhz/2mhz)   
        'video_timing_b = (bus_timing * 4) 
        'video_timing_w = (bus_timing * 5) 
        'video_timing_l = (bus_timing * 10) 
          
        'if (cpu_16bitbus) Then video_timing_l = video_timing_w * 2 
End Sub



Sub resetvideo() 
        Dim As Integer c 

        For c=18 To 63: crtcmask(c)=&hFF : next 
 
 		  cgastat=0
 		  
        crtc(0)=&h38 
        crtc(1)=&h28 
        crtc(4)=&h7F 
        crtc(5)=&h06 
        crtc(6)=&h64 
        crtc(7)=&h70 
        crtc(8)=&h02 
        crtc(9)=1 
        
End Sub
