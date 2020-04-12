Declare Sub mouse_serial_rcr()


Declare Sub serial1_init(addr As UShort)  
Declare Sub serial2_init(addr As UShort)  
Declare Sub serial1_remove()  
Declare Sub serial2_remove()  
Declare Sub serial_reset()  

Type serial0
     As UByte linestat,thr,mctrl,rcr,iir,ier,lcr
     As UByte dlab1,dlab2
End Type
static shared As serial0 serial, serial2

static shared As Integer serial_fifo_read, serial_fifo_write

static shared As Integer mousepos=-1
static shared As Integer mousedelay
static shared as UByte serial_fifo(0 To 255)


Sub serial_reset() 
        serial.iir=0
        serial.ier=0
        serial.lcr=0 
        serial2.iir=0
        serial2.ier=0
        serial2.lcr=0 
        mousedelay=0 
        serial_fifo_read = 0
        serial_fifo_write = 0 
End Sub

Sub serial_write_fifo(dat As UByte ) 
        serial_fifo(serial_fifo_write) = dat 
        serial_fifo_write = (serial_fifo_write + 1)  And &hFF 
        if (serial.linestat  And 1)=0 Then 
                serial.linestat = serial.linestat Or 1 
                if (serial.mctrl And 8) Then picint(&h10) 
                serial.iir=4 
        EndIf
End Sub

Function serial_read_fifo() As UByte 
        Dim As UByte  temp = serial_fifo(serial_fifo_read) 
        if serial_fifo_read <> serial_fifo_write Then serial_fifo_read = (serial_fifo_read + 1)  And &hFF 
        return temp 
End Function

Sub sendserial(dat As UByte ) 
        serial.rcr=dat 
        serial.linestat = serial.linestat Or 1 
        if serial.mctrl And 8 Then picint(&h10) 
        serial.iir=4 
End Sub

Sub serial_write(addr As UShort , valor As UByte ) 
        Select Case As Const (addr And 7)
        	Case 0 
                if serial.lcr And &h80 Then 
                        serial.dlab1=valor 
                        return 
                EndIf
                serial.thr=valor 
                serial.linestat = serial.linestat Or &h20 
                if (serial.mctrl And &h10) Then 
                        serial_write_fifo(valor) 
                EndIf
                
        	Case 1 
                if serial.lcr And &h80 Then 
                        serial.dlab2=valor 
                        return 
                EndIf
                serial.ier=valor 
                
        	case 3  
        		serial.lcr=valor 
        		
        	Case 4 
                if ((valor And 2)<>0)  And  ((serial.mctrl And 2)=0) Then 
                        'if (serial_rcr) Then serial_rcr() 
                        'serial_rcr()
                        mouse_serial_rcr()
                        'Print #5,"Serial receiver sin hacer"
                EndIf
                serial.mctrl=valor 
                
       End Select
end Sub

Function serial_read(addr As UShort ) As UByte 
        Dim As UByte  temp 
        Select Case As Const (addr And 7)
        	Case 0 
                if serial.lcr And &h80 Then return serial.dlab1 
                serial.iir=1 
                serial.linestat = serial.linestat And inv(1)
                temp=serial_read_fifo() 
                if serial_fifo_read <> serial_fifo_write Then 
                        mousepos = 0 
                        mousedelay = 1000 
                EndIf
                
        	Case 1 
                if serial.lcr And &h80 Then 
                        temp = serial.dlab2 
                else
                        temp = 0
                EndIf
                
        	case 2  
        		temp=serial.iir 
        		
        	case 3  
        		temp=serial.lcr 
        		
        	case 4  
        		temp=serial.mctrl 
        		
        	case 5  
        		temp=serial.linestat
        		serial.linestat = serial.linestat Or &h60 
        		
        	Case Else
        		temp=0 
        		
       End Select

        return temp 
End Function

Sub serial2_write(addr As UShort , valor As UByte ) 
        Select Case As Const (addr And 7)
        	Case 0 
                if serial2.lcr And &h80 Then 
                        serial2.dlab1=valor 
                        return 
                EndIf
                serial2.thr=valor 
                serial2.linestat = serial2.linestat Or &h20 
                if serial2.mctrl And &h10 Then 
                        serial2.rcr=valor 
                        serial2.linestat = serial2.linestat Or 1 
                EndIf
                
        	Case 1 
                if serial2.lcr And &h80 Then 
                        serial2.dlab2=valor 
                        return 
                EndIf
                serial2.ier=valor 
                
        	case 3  
        		serial2.lcr=valor 
        		
        	Case 4 
            serial2.mctrl=valor 
                
       End Select
end Sub

Function serial2_read(addr As UShort ) As UByte 
        Dim As UByte  temp =Any
        Select Case As const (addr And 7)
        	Case 0 
             if serial2.lcr And &h80 Then return serial2.dlab1 
             serial2.iir=1 
             serial2.linestat = serial2.linestat And inv(1) 
             temp=serial2.rcr 
        	Case 1 
             If serial2.lcr And &h80 Then return serial2.dlab2 
             temp=0 
        	Case 2  
        		temp=serial2.iir 
        	Case 3  
        		temp=serial2.lcr 
        	Case 4  
        		temp=serial2.mctrl 
        	Case 5  
        		temp=serial2.linestat 
        	Case Else
        		temp=0 
        End Select
        return temp 
End Function
