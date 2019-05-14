Type colorRGB
    As ubyte r, g, b
End Type

Type RGB_EGA
        As ubyte r, g, b
End Type    
Dim Shared As RGB_EGA vgapal(256)

' esta variable me la invento: es para meter las lineas dibujadas por el modulo SVGA
Dim Shared As ULong vga_buffer(2000,2000) 

' variable que desactiva la VGA a peticion de la SVGA ET4000
Dim Shared As Integer vga_ports_activos


Dim Shared as Integer changeframecount=2 
Dim Shared as Integer svgaon=0
Dim Shared as Ulong svgarbank
Dim Shared As ULong svgawbank
Dim Shared as UByte svgaseg ' ET4000
Dim Shared As UByte svgaseg2 ' ET4000_W32
Dim Shared as UByte cgastat 
Dim Shared as UByte gdcreg(16)
Dim Shared as Integer gdcaddr
Dim Shared as UByte attrregs(32)
Dim Shared as integer attraddr
Dim Shared As Integer attrff=0
Dim Shared as UByte rotatevga(8, 256)

Dim Shared as UByte colourcompare,colournocare

Dim Shared as UByte seqregs(32)
Dim Shared as Integer seqaddr

Dim Shared as ULong Ptr pallook
Dim Shared As ULong pallook16(256),pallook64(256) ' estas paletas son para EGA solo
Dim Shared As ULong pallook256(256) ' paleta VGA

Dim Shared as Integer dacread,dacwrite,dacpos,dacmask,dacstatus
Dim Shared as Integer fullchange ' si cambia algun dato en la SVGA, esta se actualiza
Dim Shared as double dispontime,dispofftime,disptime
Dim Shared as Integer vidclock ' guarda el registro de velocidad SVGA: 0=VGACONST1, sino VGACONST2
'
Dim Shared as Integer bpp
Dim Shared as ULong vrammask
Dim Shared as UByte changedvram((8192*1024)/1024)
Dim Shared as Integer charseta,charsetb
Dim Shared as UByte writemask,charset
Dim Shared as Integer writemode,readmode,readplane,chain4
'
Dim Shared as Integer egapal(16)
Dim Shared as Integer displine
Dim Shared as Integer scrblank=0
Dim Shared as UByte edatlookup(4, 4)
Dim Shared as Integer olddisplines,oldxsize
Dim Shared as Integer wx,wy

Dim Shared As ubyte crtc(128)
Dim Shared As UByte crtcreg

' aqui se inicializan solo 18 registros, el resto, se ponen a FF en "resetvideo()"
Dim Shared As ubyte crtcmask(128)={&hFF,&hFF,&hFF,&hFF,&h7F,&h1F,&h7F,&h7F,&hF3,&h1F,&h7F,&h1F,&h3F,&hFF,&h3F,&hFF,&hFF,&hFF}

Dim Shared as Integer video_timing_b, video_timing_w, video_timing_l
Dim Shared as Integer video_res_x, video_res_y, video_bpp
Dim Shared as Integer mdacols(256, 2, 2)
Dim Shared As ubyte charbuffer(256)
Dim Shared as Integer oddeven=0

Dim Shared As Double isa_timing

/'Vertical timings'/
Dim Shared as Integer svga_vtotal, svga_dispend, svga_vsyncstart, svga_split
/'Horizontal timings'/
Dim Shared as Integer svga_hdisp, svga_htotal, svga_rowoffset
/'Flags - svga_lowres = 1/2 clock in 256+ colour modes, svga_interlace = interlace mode enabled'/
Dim Shared as Integer svga_lowres, svga_interlace
Dim Shared as Integer svga_hdisp_on
Dim Shared as Double svga_clock
Dim Shared as uByte svga_miscout=0
Dim Shared as Integer svga_vram_limit

' estaticas
static Shared as UByte dla,dlb,dlc,dld
static Shared as Integer vc,sc
Static Shared as Integer linepos
Static Shared as Integer scrollcache
static Shared as Integer con,cursoron,cgablink
Static Shared as Integer linecountff
Static Shared as Integer vslines
static Shared as UByte svga_rotate(8, 256)
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
