object FormAbout: TFormAbout
  Left = 339
  Top = 209
  BorderStyle = bsToolWindow
  Caption = 'About'
  ClientHeight = 195
  ClientWidth = 291
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 45
    Top = 32
    Width = 32
    Height = 32
    AutoSize = True
    Center = True
    Picture.Data = {
      055449636F6E0000010002001010100000000000280100002600000020201000
      00000000E80200004E0100002800000010000000200000000100040000000000
      C000000000000000000000000000000000000000000000000000800000800000
      0080800080000000800080008080000080808000C0C0C0000000FF0000FF0000
      00FFFF00FF000000FF00FF00FFFF0000FFFFFF00000000000000000000000000
      000000000000000000000000007777777777770007FFFFFFFFFFFF7007FFFFFF
      FFFFFF7007FFFFF88FFFFF7007FFFFF88FFFFF7007FFFFF88FFFFF7007777FFF
      FFF7777007FF7FFFFFF7FA700077777777777700000000000000000000000000
      0000000000000000000000000000000000000000FFFF0000FFFF0000FFFF0000
      C003000080010000800100008001000080010000800100008001000080010000
      C0030000FFFF0000FFFF0000FFFF0000FFFF0000280000002000000040000000
      0100040000000000800200000000000000000000000000000000000000000000
      00008000008000000080800080000000800080008080000080808000C0C0C000
      0000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF0000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000077777777777777777777777777770007FFFFFF
      FFFFFFFFFFFFFFFFFFFFFF707FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF77FFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFF77FFFFFFFFFFFFF8888FFFFFFFFFFFFF77FFFFFFF
      FFFFFF8FF8FFFFFFFFFFFFF77FFFFFFFFFFFFF8FF8FFFFFFFFFFFFF77FFFFFFF
      FFFFFF8888FFFFFFFFFFFFF77FFFFFFFFFFFFF8888FFFFFFFFFFFFF77FFFFFFF
      FFFFFF8FF8FFFFFFFFFFFFF77FFFFFFFFFFFFF8FF8FFFFFFFFFFFFF77FFFFFFF
      FFFFFF8888FFFFFFFFFFFFF77FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF77FFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFF77FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF777777777
      8888888888888888777777777FFFFFF788888888888888887FFFFAF77FFFFFF7
      FFFFFFFFFFFFFFFF7FFFFAF707FFFFF7FFFFFFFFFFFFFFFF7FFFFA7000777777
      7777777777777777777777000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC000000380000001
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      80000001C0000003FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
    Proportional = True
  end
  object Bevel1: TBevel
    Left = 8
    Top = 8
    Width = 273
    Height = 145
    Shape = bsFrame
  end
  object StaticText1: TStaticText
    Left = 85
    Top = 32
    Width = 161
    Height = 25
    Caption = '.nds Trimmer'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
  end
  object StaticText2: TStaticText
    Left = 70
    Top = 88
    Width = 103
    Height = 17
    Caption = 'Coded By: estranged'
    TabOrder = 1
  end
  object StaticText3: TStaticText
    Left = 70
    Top = 104
    Width = 60
    Height = 17
    Caption = 'Version: 0.2'
    TabOrder = 2
  end
  object StaticText4: TStaticText
    Left = 70
    Top = 120
    Width = 151
    Height = 17
    Caption = 'email: estranged07@gmail.com'
    TabOrder = 3
  end
  object BitBtn1: TBitBtn
    Left = 108
    Top = 160
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 4
    OnClick = BitBtn1Click
  end
end