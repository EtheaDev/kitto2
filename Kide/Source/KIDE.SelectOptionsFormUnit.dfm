object SelectOptionForm: TSelectOptionForm
  Left = 313
  Top = 231
  BorderStyle = bsDialog
  Caption = 'Select option'
  ClientHeight = 329
  ClientWidth = 292
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object paBottom: TPanel
    Left = 0
    Top = 292
    Width = 292
    Height = 37
    Align = alBottom
    TabOrder = 0
    object paButtons: TPanel
      Left = 120
      Top = 1
      Width = 171
      Height = 35
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object bbOK: TButton
        Left = 3
        Top = 3
        Width = 81
        Height = 29
        Caption = 'Select'
        Default = True
        ModalResult = 1
        TabOrder = 0
      end
      object bbCancel: TButton
        Left = 87
        Top = 3
        Width = 80
        Height = 29
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 1
      end
    end
  end
  object gb: TRadioGroup
    Left = 0
    Top = 0
    Width = 292
    Height = 212
    Align = alClient
    Caption = 'Options'
    TabOrder = 1
    ExplicitHeight = 292
  end
  object IconsRadioGroup: TRadioGroup
    Left = 0
    Top = 212
    Width = 292
    Height = 80
    Align = alBottom
    Caption = 'Icons style'
    ItemIndex = 0
    Items.Strings = (
      'Colored small'
      'Flat/Black small'
      'Flat/Black medium')
    TabOrder = 2
    ExplicitTop = 216
  end
end
