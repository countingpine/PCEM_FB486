Dim Shared As Integer key(256) ' guarda la pulsacion de cada tecla
Static Shared tiempo_teclado As Double

Declare Sub keyboard_at_adddata(f As ubyte)
Declare Sub keyboard_at_adddata_keyboard(valor As UByte ) 
function LeeTeclado() As Integer
    Static tecla As Integer
    Static teclashift As Integer
    Static oldtecla As Integer


	' envia la orden de "fin de pulsacion", añadiendo &h80 al codigo
	If teclashift Then keyboard_at_adddata(teclashift+128):teclashift=0
	If tecla Then keyboard_at_adddata(tecla+128):tecla=0
    
    
     If MultiKey(SC_LSHIFT) Then teclashift=&h2a

   
   If MultiKey(SC_ESCAPE) Then tecla=&h01
   If MultiKey(SC_1) Then tecla=&h02
   If MultiKey(SC_2) Then tecla=&h03
   If MultiKey(SC_3) Then tecla=&h04
   If MultiKey(SC_4) Then tecla=&h05
   If MultiKey(SC_5) Then tecla=&h06
   If MultiKey(SC_6) Then tecla=&h07
   If MultiKey(SC_7) Then tecla=&h08
   If MultiKey(SC_8) Then tecla=&h09
   If MultiKey(SC_9) Then tecla=&h0A
   If MultiKey(SC_0) Then tecla=&h0B
   If MultiKey(SC_MINUS) Then tecla=&h0C
   If MultiKey(SC_EQUALS) Then tecla=&h0D
   If MultiKey(SC_BACKSPACE) Then tecla=&h0E
   If MultiKey(SC_TAB) Then tecla=&h0F
   If MultiKey(SC_Q) Then tecla=&h10
   If MultiKey(SC_W) Then tecla=&h11
   If MultiKey(SC_E) Then tecla=&h12
   If MultiKey(SC_R) Then tecla=&h13
   If MultiKey(SC_T) Then tecla=&h14
   If MultiKey(SC_Y) Then tecla=&h15
   If MultiKey(SC_U) Then tecla=&h16
   If MultiKey(SC_I) Then tecla=&h17
   If MultiKey(SC_O) Then tecla=&h18
   If MultiKey(SC_P) Then tecla=&h19
   If MultiKey(SC_LEFTBRACKET) Then tecla=&h1A
   If MultiKey(SC_RIGHTBRACKET) Then tecla=&h1B
   If MultiKey(SC_ENTER) Then tecla=&h1C
   If MultiKey(SC_CONTROL) Then tecla=&h1D
   If MultiKey(SC_A) Then tecla=&h1E
   If MultiKey(SC_S) Then tecla=&h1F
   If MultiKey(SC_D) Then tecla=&h20
   If MultiKey(SC_F) Then tecla=&h21
   If MultiKey(SC_G) Then tecla=&h22
   If MultiKey(SC_H) Then tecla=&h23
   If MultiKey(SC_J) Then tecla=&h24
   If MultiKey(SC_K) Then tecla=&h25
   If MultiKey(SC_L) Then tecla=&h26
   If MultiKey(SC_SEMICOLON) Then tecla=&h27
   If MultiKey(SC_QUOTE) Then tecla=&h28
   If MultiKey(SC_TILDE) Then tecla=&h29
   'If MultiKey(SC_LSHIFT) Then tecla=&h2A
   If MultiKey(SC_BACKSLASH) Then tecla=&h2B
   If MultiKey(SC_Z) Then tecla=&h2C
   If MultiKey(SC_X) Then tecla=&h2D
   If MultiKey(SC_C) Then tecla=&h2E
   If MultiKey(SC_V) Then tecla=&h2F
   If MultiKey(SC_B) Then tecla=&h30
   If MultiKey(SC_N) Then tecla=&h31
   If MultiKey(SC_M) Then tecla=&h32
   If MultiKey(SC_COMMA) Then tecla=&h33
   If MultiKey(SC_PERIOD) Then tecla=&h34
   If MultiKey(SC_SLASH) Then tecla=&h35
   If MultiKey(SC_RSHIFT) Then tecla=&h36
   If MultiKey(SC_MULTIPLY) Then tecla=&h37
   If MultiKey(SC_ALT) Then tecla=&h38
   If MultiKey(SC_SPACE) Then tecla=&h39
   If MultiKey(SC_CAPSLOCK) Then tecla=&h3A
   If MultiKey(SC_F1) Then tecla=&h3B
   If MultiKey(SC_F2) Then tecla=&h3C
   If MultiKey(SC_F3) Then tecla=&h3D
   If MultiKey(SC_F4) Then tecla=&h3E
   If MultiKey(SC_F5) Then tecla=&h3F
   If MultiKey(SC_F6) Then tecla=&h40
   If MultiKey(SC_F7) Then tecla=&h41
   If MultiKey(SC_F8) Then tecla=&h42
   If MultiKey(SC_F9) Then tecla=&h43
   If MultiKey(SC_F10) Then tecla=&h44
   If MultiKey(SC_NUMLOCK) Then tecla=&h45
   If MultiKey(SC_SCROLLLOCK) Then tecla=&h46
   If MultiKey(SC_HOME) Then tecla=&h47
   If MultiKey(SC_UP) Then tecla=&h48
   If MultiKey(SC_PAGEUP) Then tecla=&h49
   If MultiKey(SC_LEFT) Then tecla=&h4B
   If MultiKey(SC_RIGHT) Then tecla=&h4D
   If MultiKey(SC_PLUS) Then tecla=&h4E
   If MultiKey(SC_END) Then tecla=&h4F
   If MultiKey(SC_DOWN) Then tecla=&h50
   If MultiKey(SC_PAGEDOWN) Then tecla=&h51
   If MultiKey(SC_INSERT) Then tecla=&h52
   If MultiKey(SC_DELETE) Then tecla=&h53
   If MultiKey(SC_F11) Then tecla=&h57
   If MultiKey(SC_F12) Then tecla=&h58
   If MultiKey(SC_LWIN) Then tecla=&h7D
   If MultiKey(SC_RWIN) Then tecla=&h7E
   If MultiKey(SC_MENU) Then tecla=&h7F
   
   
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''   
   ' rutina para tratar de reproducir la repeticion de teclas
	If tiempo_teclado=1000 Then 
		If oldtecla=tecla Then GoTo aaa
		tiempo_teclado=0
		tecla=0: Return 0
	EndIf

   ' evito que se pulse la misma tecla varias veces, pero no funciona el autorepetir
   If oldtecla=tecla And oldtecla>0 Then 
   	If (tiempo_teclado>0) Then
   		If (Timer-tiempo_teclado)<0.4 Then ' pausa antes de repeticion, menos de 0.1 falla
   			tecla=0: Return 0
   		Else
   		   tiempo_teclado=1000 'Timer   	
   		   GoTo aaa	 
   		End if
   	Else
	   	If tiempo_teclado=0 Then tiempo_teclado=Timer
	   	tecla=0: Return 0
   	End If
   EndIf
   tiempo_teclado=0
aaa:
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

	If teclashift Then keyboard_at_adddata(teclashift)
	If tecla Then keyboard_at_adddata(tecla)

   oldtecla=tecla
   
   Return tecla
End Function


