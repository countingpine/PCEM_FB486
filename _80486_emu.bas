' EMULADOR 80486 DX2-66 CON TSENG LABS SVGA , POR JOSEBA EPALZA 2019 (JEPALZA @ GMAIL dot COM)
' EMPLEANDO CODIGO FUENTE "PCEM VERSION 8" ( https://pcem-emulator.co.uk/ )

' pantalla grafica
ScreenRes 800,600,32,2
ScreenSet 1,0
Static Shared As any Ptr scrbuffer 
Static Shared As ulong Ptr pixel
scrbuffer = ScreenPtr()


' para el MULTIKEY
#include "fbgfx.bi"
#if __FB_LANG__ = "fb"
Using FB 
#EndIf

 ' necesario para detectar las teclas ALT que el KEYEVENT no detecta
#Include "windows.bi"

' variables para depurar solo
static shared As integer mitemp
Static shared As Integer deb=0
static shared As Integer skipnextprint

' salida a consola
Open cons For Output As 5

' depuracion
declare sub printdebug()



' variables y declaraciones
#Include "ibm.bi"



''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' RAM 
Const RAM_SIZE  =16 ' megas
Const RAM_TOTAL =((RAM_SIZE*1024*1024)-1) ' el total de ram segun RAM_SIZE, empezando en 0, para que acabe en XFFFF
' VRAM
Const VRAM_SIZE  =2 ' megas
Const VRAM_TOTAL =((VRAM_SIZE*1024*1024)-1)

' memoria
static shared As UByte rambuf(0 To RAM_TOTAL) 
static shared As UByte rombuf(0 To &h1FFFF) 
static shared As UByte vrambuf(0 To VRAM_TOTAL) 
static shared As UByte vrombuf(0 To &h7FFF) 
'static shared As UByte rambiosbuf(&h10000) ' inventado por mi
' 
ram=@rambuf(0)
rom=@rombuf(0)
vram=@vrambuf(0)
vrom=@vrombuf(0)
'rambios=@rambiosbuf(0) ' inventado por mi

' cache
static shared As ULong readlookup2buf (0 To (1024*1024)-1)
static shared As ULong writelookup2buf(0 To (1024*1024)-1)
static shared As UByte cachelookup2buf(0 To (1024*1024)-1) 

readlookup2 =@readlookup2buf(0)
writelookup2=@writelookup2buf(0)
cachelookup2=@cachelookup2buf(0)

' marcamos zonas RAM utilizables (menos VRAM), y descartamos ROMS
Dim As Integer f=0
for f=0 To (RAM_SIZE*16)-1
       isram(f)=1
       if (f >= &ha and f<=&hF) Then isram(f)=0 ' entre a0000 y f0000 NO es ram
       ' por ejemplo A0000 es la VRAM, entre E y F es BIOS, la C es VROM
Next
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''






#Include "IDE.bas"
#Include "Keyb.bas"
#Include "serial.bas"
#Include "Mouse.bas"
'
#Include "Video.bas"
#Include "EGA.bas"
#Include "vid_svga.bas"
#Include "vid_et4000.bas"
'
#Include "i8259_pic.bas"
#Include "i8253_pit.bas"
#Include "ali1429.bas"
#Include "mem.bas"
#Include "DMA.bas"
#Include "Debug.bas"
#Include "keyboard_at.bas"
#Include "x86seg.bas"
#Include "x86.bas"
#Include "x87.bas"
#Include "IO.bas"
#Include "386a.bas" ' este incluye a su vez el B y el C




' esto es solo para mostrar las INS al depurar en la rutina PRINTDEBUG
initoplist()

' ----------------- DEPURACION ---------------------
DEB=0 ' 1=normal sin pausa, 2=normal CON pausa, 3= solo textos del emulador







''''''''''''''''''''''''''''''''''''''''''''''''  BIOS ''''''''''''''''''''''''''''''''''''''''''''''''''''

'''''''''''''''''
' por ahora, la config mas chula es la de 16mb de RAM, 2mb VRAM, bios ALI1429 y TSENG ET4K_W32 
'''''''''''''''''

' VGA
'loadbinary &h0000, "video\tseng_et4000_04-92.bin", vrom
'loadbinary &h0000, "video\et4000.bin", vrom
'loadbinary &h0000, "video\BIOS_v0.26.bin", vrom ' TSENLABS ET6000 con 4mb de VRAM!!!! es del 97, la mas moderna!!!
loadbinary &h0000, "video\ET4K_W32.bin", vrom ' con 2mb de VRAM : esta permite llegar a 800x600 16 colores en el DP

' AMIBIOS
'init_PC("ami486")
'loadbinary &h0000, "bios\ami486.bin", rom 

init_PC("ali1429g") 
loadbinary &h0000, "bios\ali1429g.bin", rom 

'loadbinary &h0000, "bios\test.bin", rom 





''''''''''''''''''''''''''''''''''''''''''''''   DISCO DURO (HD) '''''''''''''''''''''''''''''''''''''''''
loadhd("HDD\msdos622.img",512,63,16)  ' disco 512 cilindros,63 sectores, 16 cabezas, 256 mb
'loadhd("HDD\msdos622_qemm.img",1580,63,16)  ' disco 1580 cilindros,63 sectores, 16 cabezas, 800 mb
'loadhd("HDD\minix.img",1255,63,16) ' el 1255 es desconocido, pero con este funciona
'loadhd("HDD\WIN95.img",512,63,16) ' el 1255 es desconocido, pero con este funciona

' DISCO FLEXIBLE , no funciona bien, eliminado por el momento.
'loaddisc("FDC\msdos622.IMA",0)




Print #5,"Teclas especiales:"
Print #5,"<F10> Captura o Devuelve el Raton a nuestra ventana"
Print #5,"<F11> Sale del emulador guardando datos de memoria."
Print #5,"<F12> Modo depuracion"
Print #5,"Si salimos picando la 'X' de la ventana de comandos, sale sin guardar"
Print #5,""



''''''''''''''''''''''''''''''''''
''''''''' PRINCIPAL ''''''''''''''
''''''''''''''''''''''''''''''''''

Open "_salida.txt" For Output As 1


If hasfpu=0 Then Print #5,"SIN FPU !!!!"
If IDE_GRABA=1 Then Print #5,"ATENCION!!! IDE permite grabar en el DISCO DURO VIRTUAL ....."

Dim fps As Double

cpu_exec = CPUCLOCK\100

Var ratoncapturado=1
While 1
	
 'fps=Timer
 
 exec386(cpu_exec)


  ' capturar o devolver el raton de windows a nuestra ventana
  If MultiKey(SC_F10) Then 
  	If ratoncapturado=0 Then 
  		ratoncapturado=1
  		SetMouse ,,0,1 ' captura el raton
  		Sleep 300,1
  	Else
  		ratoncapturado=0
  		SetMouse ,,1,0 ' devuelve el raton
  		Sleep 300,1
  	EndIf
  EndIf
  
  If MultiKey(SC_F11) Then GoTo ya ' F11 para salir
  If MultiKey(SC_F12) Then deb=2

	'tiempo_teclado=Timer
	f=leeteclado()
   mouse_poll()
   
   'Print #5,Int((Timer-fps)*100);

Wend

' si pulsamos ESC salimos "sin mas"
Close 1
end

' si pulsamos F11 salimos aqui, y se permite guardar datos de memoria
ya:
Close 1





''''''''''''''''''''''''''''''''''''''  final con salida de datos opcional '''''''''''''''''''''''''''''''''
' depuracion, salida de datos
Print #5,""
Print #5, "Guardando datos de memoria. Paciencia, que son muchos!!!"
Print #5,""
Print #5," Guardando RAM (16mb)"
Open "salida_ram_16mb.bin" For Binary Access write As 1
For f=0 To RAM_TOTAL-1
	Put #1, f+1, Chr(ram[f])
Next
Close 1
Print #5," Guardando VRAM 'A0000' 800x600"
Open "salida_vram_800x600.bin" For Binary Access write As 1
For f=0 To (65535*4)
	Put #1, f+1, Chr(vram[f])
Next
Close 1
Print #5," Guardando 'B0000'"
Open "salida_B0000-C0000.bin" For Binary Access write As 1
For f=0 To 131071 'RAM_TOTAL-1
	Put #1, f+1, Chr(ram[f+&HB0000])
Next
Close 1