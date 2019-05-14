
'io_sethandler(&h0022, &h0002, ali1429_read, NULL, NULL, ali1429_write, NULL, NULL) 

declare Sub flushmmucache()

Static Shared as Integer ali1429_index
static Shared as Byte ali1429_regs(256)

Sub ali1429_write(port As UShort , valor As UByte ) 
	
        if (port And 1)=0 Then 
          ali1429_index=valor 
        else
          ali1429_regs(ali1429_index)=valor 
          Select Case As Const  (ali1429_index)
          	case &h13 
                 if (valor And &hC0)=0 Then 
                        shadowbios=0 
                         'Print #5,"ali1429_write case 13"
                         If ( shadowbios_write=0) Then 
                         	grabar_bios=0
                         	'modo_bios=0
                         '   mem_sethandler(&hf0000, &h10000, mem_read_bios,   mem_read_biosw,   mem_read_biosl,   NULL,          NULL,           NULL          ) 
                         Else
                         	grabar_bios=1
                         	'modo_bios=2
                         '   mem_sethandler(&hf0000, &h10000, mem_read_bios,   mem_read_biosw,   mem_read_biosl,   mem_write_ram, mem_write_ramw, mem_write_raml)
                         EndIf
                         flushmmucache() 
                 EndIf

          	Case &h14 
               shadowbios=valor And 1
               shadowbios_write=valor And 2 
               'print #5,"ali1429_write case 14:";valor And 3
               Select Case As Const  (valor  And 3)
               	Case 0 
               		modo_bios=0
               '		mem_sethandler(&hf0000, &h10000, mem_read_bios,   mem_read_biosw,   mem_read_biosl,   NULL,          NULL,           NULL          )
               	Case 1 
               		modo_bios=1
               '		mem_sethandler(&hf0000, &h10000, mem_read_ram,    mem_read_ramw,    mem_read_raml,    NULL,          NULL,           NULL          )
               	Case 2 
               		modo_bios=2
               '		mem_sethandler(&hf0000, &h10000, mem_read_bios,   mem_read_biosw,   mem_read_biosl,   mem_write_ram, mem_write_ramw, mem_write_raml)
               	Case 3 
               		modo_bios=3
               '		mem_sethandler(&hf0000, &h10000, mem_read_ram,    mem_read_ramw,    mem_read_raml,    mem_write_ram, mem_write_ramw, mem_write_raml)
               End Select
               flushmmucache() 

         End Select
        EndIf
End Sub

Function ali1429_read(port As UShort ) As UByte 
        if (port And 1)=0 Then return ali1429_index 
        'print #5,"ALI1429_read"
        'if ((ali1429_index >= &hc0  Or  ali1429_index = &h20)  And  cpu_iscyrix) Then return &hff: 'Dont conflict with Cyrix config registers
        return ali1429_regs(ali1429_index) 
End Function

Sub ali1429_reset() 
        Dim As Integer c
        For c=0 To 255
        	 ali1429_regs(c)=&hff
        Next
End Sub


