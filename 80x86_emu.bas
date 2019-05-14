' EMULADOR 80486 DX2-66 CON TSENG LABS SVGA , POR JOSEBA EPALZA 2019 (JEPALZA@GMAIL.COM)
' EMPLEANDO CODIGO FUENTE "PCEM VERSION 8" ( https://pcem-emulator.co.uk/ )

Screen 19,16,2 ' si quito el 16, el debug no se ve bien por los colores RGB ponidos por mi
ScreenSet 1,0

' para el MULTIKEY
#include "fbgfx.bi"
#if __FB_LANG__ = "fb"
Using FB 
#EndIf

 ' necesario para detectar las teclas ALT que el KEYEVENT no detecta
#Include "windows.bi"

' variables a borrar cuando este depurado
Dim Shared As integer mitemp

' variables depuracion
Dim Shared as Integer deb=0
Dim Shared as Integer skipnextprint
' salida a consola
Open cons For Output As 5

' depuracion
declare sub printdebug()



' variables y declaraciones
#Include "ibm.bi"



''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' RAM 
#define RAM_SIZE 16 ' megas
#Define RAM_TOTAL ((RAM_SIZE*1024*1024)-1) ' el total de ram segun RAM_SIZE, empezando en 0, para que acabe en XFFFF
' VRAM
#define VRAM_SIZE 2 ' megas
#Define VRAM_TOTAL ((VRAM_SIZE*1024*1024)-1)

' memoria
Dim shared As UByte rambuf(RAM_TOTAL+1) 
Dim Shared As UByte rombuf(&h20000) 
Dim Shared As UByte vrambuf(VRAM_TOTAL+1) 
Dim Shared As UByte vrombufc(&h8000) 
'Dim Shared As UByte rambiosbuf(&h10000) ' inventado por mi
' 
ram=@rambuf(0)
rom=@rombuf(0)
vram=@vrambuf(0)
vrom=@vrombufc(0)
'rambios=@rambiosbuf(0) ' inventado por mi

' cache
Dim Shared As ULong readlookup2buf (1024*1024)
Dim Shared As ULong writelookup2buf(1024*1024)
Dim shared As UByte cachelookup2buf(1024*1024) 

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

DEB=0 ' 1=normal sin pausa, 2=normal CON pausa, 3= solo textos del emulador

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



''''''''''''''''''''''''''''''''''
''''''''' PRINCIPAL ''''''''''''''
''''''''''''''''''''''''''''''''''

loadhd("HDD\msdos622.img",1580,63,16)  ' disco 1580 cilindros,63 sectores, 16 cabezas, 800 mb
'loadhd("HDD\msdos622_qemm.img",1580,63,16)  ' disco 1580 cilindros,63 sectores, 16 cabezas, 800 mb
'loadhd("HDD\minix.img",1255,63,16) ' el 1255 es inventado

'loaddisc("FDC\msdos622.IMA",0)


Open "_salida.txt" For Output As 1


If hasfpu=0 Then Print #5,"SIN FPU !!!!"
If IDE_GRABA=1 Then Print #5," ATENCION!!! IDE permite grabar....."


cpu_exec = CPUCLOCK\100
'SetMouse ,,0,1 ' esconde el raton y lo limita a la ventana
While 1

 exec386(cpu_exec)

  If MultiKey(SC_F12) Then deb=2
  'If MultiKey(SC_F10) Then SetMouse ,,1,0 ' devuevle el raton
  If MultiKey(SC_F11) Then GoTo ya ' F11 para salir

	'tiempo_teclado=Timer
	f=leeteclado()
   mouse_poll()

wend
ya:
Close 1
'Dim As Integer a,b
'Open "pepe.txt" For Output As 1
'	For a=0 To 599
'		For b=0 To 799
'			Print #1,a,b,vga_buffer(a,b)
'		Next
'	Next	
'Close 1

'Open "salida_ram.bin" For Binary Access write As 1
'For f=0 To RAM_TOTAL-1
'	Put #1, f+1, Chr(ram[f])
'Next
'Close 1
'
'Open "salida_vram.bin" For Binary Access write As 1
'For f=0 To (65535*4)
'	Put #1, f+1, Chr(vram[f])
'Next
'Close 1

'Open "salida_B0000-C0000.bin" For Binary Access write As 1
'For f=0 To 131071 'RAM_TOTAL-1
'	Put #1, f+1, Chr(ram[f+&HB0000])
'Next
'Close 1