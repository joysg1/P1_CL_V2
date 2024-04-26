Public Class Parcial_1

    

    Private Function DecimalABinario(ByVal numero As Integer) As String
        Dim resto As String = ""

        Do Until numero < 2
            resto = resto & Str(numero Mod 2)
            numero = Int(numero / 2)
        Loop

        resto = resto & numero

        Return StrReverse(resto)
    End Function

    Private Function DecimalAOctal(ByVal numero As Integer) As String
        Dim resto As String = ""

        Do Until numero < 8
            resto = resto & Str(numero Mod 8)
            numero = Int(numero / 8)
        Loop

        resto = resto & numero

        Return StrReverse(resto)
    End Function

    Private Function DecimalAHexadecimal(ByVal numero As Integer) As String
        Dim resto As String = ""
        Dim hexDigits As String = "0123456789ABCDEF"

        Do Until numero < 16
            resto = resto & hexDigits.Chars(numero Mod 16)
            numero = Int(numero / 16)
        Loop

        resto = resto & hexDigits.Chars(numero)

        Return StrReverse(resto)
    End Function

    Private Sub ConvertirADecimal()
        If TextBox1.Text = "" Or TextBox1.Text <= 0 Then
            MessageBox.Show("Por favor valide el valor ingresado")
        Else
            Dim numero As Integer = TextBox1.Text

            Label1.Text = "Valor en binario: " + DecimalABinario(numero)
            Label2.Text = "Valor en octal: " + DecimalAOctal(numero)
            Label3.Text = "Valor en hexadecimal: " + DecimalAHexadecimal(numero)
        End If
    End Sub

    Private Sub Button1_Click(ByVal sender As Object, ByVal e As EventArgs) Handles Button1.Click
        ConvertirADecimal()
    End Sub



    Private Function EsBinario(ByVal valor As String) As Boolean
        For Each caracter As Char In valor
            If Not (caracter = "0" Or caracter = "1") Then
                Return False
            End If
        Next
        Return True
    End Function

    Private Function ComplementoA2(ByVal binario As String) As String
        Dim complemento As String = ""
        Dim carry As Boolean = True

        For i As Integer = binario.Length - 1 To 0 Step -1
            If binario(i) = "1" And carry Then
                complemento = "0" & complemento
            ElseIf binario(i) = "0" And carry Then
                complemento = "1" & complemento
                carry = False
            Else
                complemento = binario(i) & complemento
            End If
        Next

        Return complemento
    End Function

    Private Function RestaBinaria(ByVal minuendo As String, ByVal sustraendo As String) As String
        Dim resultado As String = ""
        Dim complementoSustraendo As String = ComplementoA2(sustraendo)

        For i As Integer = minuendo.Length - 1 To 0 Step -1
            If minuendo(i) = sustraendo(i) Then
                resultado = "0" & resultado
            Else
                resultado = "1" & resultado
            End If
        Next

        Return resultado
    End Function

    Private Sub Button2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click
        If Not EsBinario(TextBox2.Text) Then
            MessageBox.Show("Por favor, ingrese solo números binarios en el minuendo.")
            Return
        End If

        If Not EsBinario(TextBox3.Text) Then
            MessageBox.Show("Por favor, ingrese solo números binarios en el sustraendo.")
            Return
        End If

        Dim minuendo As String = TextBox2.Text
        Dim sustraendo As String = TextBox3.Text

        ' Asegurarse de que ambos números tienen la misma longitud
        Dim longitudMaxima As Integer = Math.Max(minuendo.Length, sustraendo.Length)
        minuendo = minuendo.PadLeft(longitudMaxima, "0"c)
        sustraendo = sustraendo.PadLeft(longitudMaxima, "0"c)

        ' Realizar la resta binaria utilizando el complemento a 2 del sustraendo
        Dim resultado As String = RestaBinaria(minuendo, sustraendo)

        Label7.Text = "Resultado de la resta binaria: " & resultado
    End Sub
    

    Private Function EsBinarioDe5Bits(ByVal valor As String) As Boolean
        If valor.Length > 5 Then
            Return False
        End If

        For Each caracter As Char In valor
            If Not (caracter = "0" Or caracter = "1") Then
                Return False
            End If
        Next
        Return True
    End Function


    Private Function BinarioAGray(ByVal binario As String) As String
        Dim gray As String = binario(0)

        For i As Integer = 1 To binario.Length - 1
            If binario(i) <> binario(i - 1) Then
                gray &= "1"
            Else
                gray &= "0"
            End If
        Next

        Return gray
    End Function
    

    Private Sub Button3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button3.Click
        Dim binario As String = TextBox4.Text

        If Not EsBinarioDe5Bits(binario) Then
            MessageBox.Show("Por favor, ingrese un número binario de máximo 5 bits.")
            Return
        End If

        Dim gray As String = BinarioAGray(binario)
        Label9.Text = "Código de Gray equivalente: " & gray
    End Sub

    
    








    

    Private Function IsValidBinary(ByVal binary As String) As Boolean
        ' Verificar si la cadena es un número binario válido de máximo 10 bits y no es cero
        If Not (binary.All(Function(c) c = "0" Or c = "1") AndAlso binary.Length > 0 AndAlso binary.Length <= 10) Then
            Return False ' No es válido
        End If

        Return True ' Es válido
    End Function


    Private Function CalcularGradoPolinomio(ByVal binario As String) As Integer
        ' El grado del polinomio es igual a la posición del bit más significativo (contando desde 0)
        ' Por lo tanto, el grado es igual a la longitud del binario menos 1
        Return binario.Length - 1
    End Function



    Private Sub Button4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button4.Click
        ' Verificar si el contenido de TextBox5 es un número binario válido de máximo 10 bits y no es cero
        If Not IsValidBinary(TextBox5.Text) Then
            MessageBox.Show("Por favor, ingrese un número binario válido de máximo 10 bits en el primer cuadro de texto (TextBox5).", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
            TextBox5.Focus() ' Colocar el foco en TextBox5
            Return ' Salir del subprocedimiento
        End If

        ' Verificar si el contenido de TextBox6 es un número binario válido de máximo 10 bits y no es cero
        If Not IsValidBinary(TextBox6.Text) Then
            MessageBox.Show("Por favor, ingrese un número binario válido de máximo 10 bits en el segundo cuadro de texto (TextBox6).", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
            TextBox6.Focus() ' Colocar el foco en TextBox6
            Return ' Salir del subprocedimiento
        End If


        Dim gradoPolinomio As Integer = CalcularGradoPolinomio(TextBox6.Text)

        ' Mostrar el grado del polinomio en el Label12
        Label12.Text = gradoPolinomio


        ' Polinomio generador

        Label13.Text = TextBox6.Text

        Dim ceros As String = New String("0"c, gradoPolinomio)



        Dim tA As String = TextBox5.Text + ceros



        ' Trama + a (numero de ceros segun el grado)

        Label14.Text = tA



        ' Falta parte division --- continuar








    End Sub

   
End Class


'datos de prueba informacion = 1011101001  polinomio 1101  --- corregir
