object FormVersionLog: TFormVersionLog
  Left = 380
  Top = 218
  BorderStyle = bsToolWindow
  Caption = 'Version Log'
  ClientHeight = 268
  ClientWidth = 242
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
  object Memo1: TMemo
    Left = 8
    Top = 8
    Width = 225
    Height = 209
    TabStop = False
    Lines.Strings = (
      'v0.2'
      '--------------------'
      '-Added ROM header application'
      ' size scan.'
      '-Added graphical status.'
      '-Added options dialog.'
      '-Improved trimming performance.'
      '-Improved memory handling.'
      ''
      'v0.1'
      '--------------------'
      '-Initial release.'
      '-EOF trimming capability.')
    ReadOnly = True
    TabOrder = 1
  end
  object BitBtn1: TBitBtn
    Left = 84
    Top = 232
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 0
    OnClick = BitBtn1Click
  end
end
