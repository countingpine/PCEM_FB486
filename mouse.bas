

' raton serie
Dim Shared as Integer oldb=0
Sub mouse_serial_poll(x As Integer , y As Integer , b As Integer ) 
        Dim As UByte  mousedat(3) 
       
        
        If (serial.ier And 1)=0 Then return 
        If (x=0) And (y=0) And (b=oldb) Then return 
        

        
        oldb=b 
        if (x>127) Then x=127 
        if (y>127) Then y=127 
        if (x<-128) Then x=-128 
        if (y<-128) Then y=-128 
        
        /'Use Microsoft format'/
        mousedat(0) = &h40 
        mousedat(0) = mousedat(0) Or (((y Shr 6) And 3) Shl 2) 
        mousedat(0) = mousedat(0) Or  ((x Shr 6) And 3) 
        
        if (b And 1) Then mousedat(0) = mousedat(0) Or &h20 
        if (b And 2) Then mousedat(0) = mousedat(0) Or &h10 
        
        mousedat(1)=x And &h3F 
        mousedat(2)=y And &h3F 
        
        if (serial.mctrl And &h10)=0 Then 
                serial_write_fifo(mousedat(0)) 
                serial_write_fifo(mousedat(1)) 
                serial_write_fifo(mousedat(2)) 
        EndIf
        
                'Locate 31,30:Print "mouse:";x,y,b
End Sub



Sub mouse_serial_rcr() 
        mousepos=-1 
        mousedelay=1000
End Sub


Dim Shared As integer pollmouse_delay = 2
Sub mouse_poll()
        Dim As integer x,y,mouse_b, relx, rely,xx,yy
        Static As Integer oldx,oldy
        
        pollmouse_delay-=1
        if (pollmouse_delay) Then Return
        pollmouse_delay = 2
        
        'poll_mouse()
        'get_mouse_mickeys(x,y)
        GetMouse (x,y,,mouse_b)
      
		' si se sale de la ventana   
		If x=-1 Or y=-1 Then
			x=oldx
			y=oldy
			xx=0
			yy=0
		EndIf
		If x<>oldx Or y<>oldy Then
			relx=oldx-x
			rely=oldy-y
			xx=-relx
			yy=-rely
			
			oldx=x
			oldy=y
		Else
			xx=0
			yy=0
		EndIf
	
        'if (mouse_poll) Then
        mouse_serial_poll(xx, yy, mouse_b)
        'if (mousecapture) thenposition_mouse(64,64)

End Sub

Sub mousecallback() 
        if (mousepos = -1) Then 
             mousepos = 0 
             serial_fifo_read = 0
             serial_fifo_write = 0 
             serial.linestat  = serial.linestat  And  inv(1)
             serial_write_fifo(Asc("M")) 
        ElseIf (serial_fifo_read <> serial_fifo_write) Then
				 serial.iir=4 
             serial.linestat = serial.linestat Or 1 
             if (serial.mctrl And 8) Then picint(&h10) 
        EndIf
End Sub
