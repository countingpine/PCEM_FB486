

'Sub pic_init() 
'        io_sethandler(&h0020, &h0002, pic_read, NULL, NULL, pic_write, NULL, NULL) 
'End Sub

'Sub pic2_init() 
'        io_sethandler(&h00a0, &h0002, pic2_read, NULL, NULL, pic2_write, NULL, NULL) 
'End Sub

Sub pic_reset() 
        pic.icw=0 
        pic.mask=&hFF 
        pic.mask2=0 
        pic.pend=0
        pic.ins=0 
        '
        pic2.icw=0 
        pic2.mask=&hFF 
        pic2.mask2=0 
        pic2.pend=0
        pic2.ins=0 
        '
        pic.vector=8 
        pic.read0=1 
        pic_intpending=0 
End Sub


Sub pic_updatepending() 
	     if ((pic2.pend And inv(pic2.mask)) And inv(pic2.mask2)) Then
                pic.pend Or= (1 shl 2)
        else
                pic.pend And= inv(1 Shl 2)
	     EndIf
        pic_intpending = (pic.pend And inv(pic.mask)) and inv(pic.mask2)
        if ((pic.mask or pic.mask2) and (1 shl 2))=0 Then
                pic_intpending = pic_intpending Or ((pic2.pend And inv(pic2.mask)) And inv(pic2.mask2))
        EndIf
End Sub

function pic_update_mask(ins As UByte) As UByte
        Dim As integer c
        Dim As UByte mask
        mask = 0
        for c = 0 To 7
                if (ins And (1 Shl c)) Then
                        mask = &hff shl c
                        Return mask
                EndIf
        Next
        Return 0
End function


Sub pic_write(ByVal addr As UShort , ByVal valor As UByte ) 
        Dim As Integer c 
        if (addr And 1) Then 
                Select Case As Const  (pic.icw)
                	Case 0  /'OCW1'/
                        pic.mask=valor 
                        pic_updatepending() 

                	Case 1  /'ICW2'/
                        pic.vector=valor And &hF8 
                        if (pic.icw1 And 2) Then 
                            pic.icw=3 
                        else
                            pic.icw=2
                        EndIf

                	Case 2  /'ICW3'/
                        if (pic.icw1 And 1) Then
                        	pic.icw=3 
                        else
                           pic.icw=0 
                        EndIf
								
                	Case 3  /'ICW4'/
                        pic.icw=0 

                End Select
        Else
                if (valor And 16) Then  /'ICW1'/
                        pic.mask=&hFF 
                        pic.mask2=0 
                        pic.icw=1 
                        pic.icw1=valor 
                        pic_updatepending() 
                ElseIf (valor And 8)=0 Then  /'OCW2'/
                        if (valor And &hE0)=&h60 Then 
                  		  pic.ins  = pic.ins And inv(1 Shl (valor And 7)) 
                          pic.mask2= pic_update_mask(pic.ins)
                          If (valor=2) And (((pic2.pend And inv(pic2.mask)) And inv(pic2.mask2))<>0) Then 
                          	 pic.pend=pic.pend Or (1 Shl 2)
                          EndIf
                          pic_updatepending() 
                        else
                          for c=0 To 7
                            if (pic.ins And (1 Shl c)) Then 
                                 pic.ins = pic.ins And inv(1 Shl c) 
                                 pic.mask2=pic_update_mask(pic.ins)
                                 If (c=2) And (((pic2.pend And inv(pic2.mask)) And inv(pic2.mask2))<>0) Then
                                 	pic.pend = pic.pend Or (1 Shl 2)
                                 EndIf
                                 pic_updatepending() 
                                 return 
                            EndIf
                         Next
                        EndIf
                else  /'OCW3'/
                        if (valor And 2) Then pic.read0=(valor And 1) 
                EndIf
        EndIf
End Sub

Function pic_read(ByVal addr As UShort ) As UByte 
        if (addr And 1) Then Return pic.mask  
        if (pic.read0)  Then return pic.ins Or IIf(pic2.ins<>0,4,0)
        return pic.pend 
End Function

Sub pic2_write(ByVal addr As UShort , ByVal valor As UByte ) 
        Dim As Integer c 
        if (addr And 1) Then 
                Select Case As Const  (pic2.icw)
                	Case 0  /'OCW1'/
                        pic2.mask=valor 
                        pic_updatepending() 

                	case 1  /'ICW2'/
                        pic2.vector=valor And &hF8 
                        if (pic2.icw1 And 2) Then 
                             pic2.icw=3 
                        else
                             pic2.icw=2
                        EndIf

                	case 2  /'ICW3'/
                        if (pic2.icw1 And 1) Then
                        	pic2.icw=3 
                        else
                           pic2.icw=0 
                        EndIf
                        
                	case 3  /'ICW4'/
                        pic2.icw=0 

                End Select
        else
                if (valor And 16) Then /'ICW1'/
                        pic2.mask=&hFF 
                        pic2.mask2=0 
                        pic2.icw=1 
                        pic2.icw1=valor 
                        pic_updatepending() 
                elseif (valor And 8)=0 Then  /'OCW2'/
                        if (valor And &hE0)=&h60 Then
                        	pic2.ins  = pic2.ins And inv(1 Shl (valor And 7)) 
                           pic2.mask2= pic_update_mask(pic2.ins)
                           pic_updatepending() 
                        else
                          for c=0 To 7
                            if (pic2.ins And (1 Shl c)) Then 
                              pic2.ins  = pic2.ins And inv(1 Shl c) 
                              pic2.mask2= pic_update_mask(pic2.ins)
                              pic_updatepending() 
                            EndIf
                          Next
                        EndIf
                else   /'OCW3'/
                        if (valor And 2) Then pic2.read0=valor And 1
                EndIf
        EndIf
End Sub

Function pic2_read(ByVal addr As UShort ) As UByte 
        if (addr And 1) Then Return pic2.mask  
        if (pic2.read0) Then return pic2.ins  
        return pic2.pend 
End Function

Sub picint(num As UShort ) 
		  If (num= (1 Shl 2)) Then num=1 Shl 9
        if (num>&hFF) Then 
          pic2.pend = pic2.pend Or (num Shr 8) 
          If ((pic2.pend And inv(pic2.mask)) And inv(pic2.mask2)) Then pic.pend=pic.pend Or (1 Shl 2)
        Else
          pic.pend = pic.pend Or num 
        EndIf
        pic_updatepending() 
End Sub


Sub picintc(num As UShort ) 
	     If num=0 Then Return
	     
        Dim As Integer c = 0 
        
        while (num And (1 Shl c))=0 : c+=1 : Wend
        
        If c=2 Then c=9:num=1 Shl 9
        
        if (num>&hFF) Then 
                pic2.pend = pic2.pend And inv(num Shr 8) 
                If ((pic2.pend And inv(pic2.mask)) And inv(pic2.mask2))=0 Then pic.pend=pic.pend Or (1 Shl 2)
        else
                pic.pend  = pic.pend And inv(num) 
        EndIf
End Sub


Function picinterrupt() As UByte 
        Dim As UByte temp, temp2
        temp=pic.pend And inv(pic.mask)
        Dim As Integer c 
        for c = 0 To 1
             if (temp And (1 Shl c)) Then 
                  pic.pend = pic.pend And inv(1 Shl c) 
                  pic.ins  = pic.ins Or (1 Shl c) 
                  pic.mask2= pic_update_mask(pic.ins)                       
                  pic_updatepending() 
                  return c+pic.vector 
             End If
        Next
        if (temp And (1 Shl 2)) Then 
             temp2 = pic2.pend And inv(pic2.mask) 
             for  c = 0 To 7
                  if (temp2 And (1 Shl c)) Then 
                       pic2.pend  = pic2.pend And inv(1 Shl c) 
                       pic2.ins   = pic2.ins Or (1 Shl c) 
                       pic2.mask2 = pic_update_mask(pic2.ins) 
                       '
                       pic.pend  = pic.pend And inv(1 Shl c) 
                       pic.ins   = pic.ins Or (1 Shl 2) /'Cascade IRQ'/
                       pic.mask2 = pic_update_mask(pic.ins) 
                       '
                       pic_updatepending() 
                       return c+pic2.vector 
                  End If
            Next
        End If
        for c = 3 To 7
             if (temp And (1 Shl c)) Then 
                  pic.pend = pic.pend And inv(1 Shl c) 
                  pic.ins  = pic.ins Or (1 Shl c) 
                  pic.mask2= pic_update_mask(pic.ins)                       
                  pic_updatepending() 
                  return c+pic.vector 
             End If
       Next
       Return &hFF 
End Function

