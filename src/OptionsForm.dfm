object FormOptions: TFormOptions
  Left = 272
  Top = 155
  BorderStyle = bsDialog
  Caption = 'Options'
  ClientHeight = 318
  ClientWidth = 474
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBoxOF: TGroupBox
    Left = 8
    Top = 104
    Width = 457
    Height = 161
    Caption = 'Output Folder'
    TabOrder = 1
    object RBSameAsROMFolder: TRadioButton
      Left = 8
      Top = 24
      Width = 441
      Height = 17
      Caption = 
        'Same folder as original ROMs (trimmed ROMs named as <original fi' +
        'le name>.x.nds)'
      Checked = True
      TabOrder = 1
      TabStop = True
      OnClick = RBSameAsROMFolderClick
    end
    object RBSpecifiedFolder: TRadioButton
      Left = 8
      Top = 48
      Width = 113
      Height = 17
      Caption = 'Specified folder:'
      TabOrder = 2
      OnClick = RBSpecifiedFolderClick
    end
    object EditFolder: TEdit
      Left = 24
      Top = 72
      Width = 400
      Height = 21
      TabStop = False
      ReadOnly = True
      TabOrder = 0
    end
    object CheckBoxOrigROMFileName: TCheckBox
      Left = 24
      Top = 104
      Width = 200
      Height = 17
      Caption = 'Keep original file name'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
    object CheckBoxRemoveUnderscores: TCheckBox
      Left = 24
      Top = 128
      Width = 200
      Height = 17
      Caption = 'Remove underscores from file name'
      TabOrder = 4
    end
  end
  object BitBtnOK: TBitBtn
    Left = 200
    Top = 280
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 2
    OnClick = BitBtnOKClick
  end
  object BitBtnOutputFolder: TBitBtn
    Left = 433
    Top = 176
    Width = 21
    Height = 21
    Caption = '...'
    Enabled = False
    TabOrder = 3
    OnClick = BitBtnOutputFolderClick
  end
  object RadioGroupTM: TRadioGroup
    Left = 8
    Top = 8
    Width = 457
    Height = 89
    Caption = 'Trim Method'
    Items.Strings = (
      'Automatic (recommended)'
      'ROM header file size scan'
      'EOF scan')
    TabOrder = 0
  end
end
