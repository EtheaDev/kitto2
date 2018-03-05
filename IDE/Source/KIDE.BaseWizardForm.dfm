inherited BaseWizardForm: TBaseWizardForm
  Left = 271
  Top = 194
  Caption = 'BaseWizardForm'
  ClientHeight = 408
  ClientWidth = 684
  ExplicitWidth = 700
  ExplicitHeight = 447
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl: TPageControl
    Left = 0
    Top = 25
    Width = 684
    Height = 347
    Align = alClient
    Style = tsFlatButtons
    TabOrder = 0
  end
  object ButtonPanel: TPanel
    Left = 0
    Top = 372
    Width = 684
    Height = 36
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      684
      36)
    object BackButton: TButton
      Left = 524
      Top = 6
      Width = 75
      Height = 25
      Action = BackAction
      Anchors = [akTop, akRight]
      TabOrder = 0
    end
    object ForwardButton: TButton
      Left = 605
      Top = 6
      Width = 75
      Height = 25
      Action = ForwardAction
      Anchors = [akTop, akRight]
      TabOrder = 1
    end
  end
  object TitlePanel: TPanel
    AlignWithMargins = True
    Left = 5
    Top = 0
    Width = 674
    Height = 25
    Margins.Left = 5
    Margins.Top = 0
    Margins.Right = 5
    Margins.Bottom = 0
    Align = alTop
    Alignment = taLeftJustify
    BevelOuter = bvNone
    Caption = 'TitlePanel'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
  end
  object ActionList: TActionList
    Left = 408
    Top = 200
    object BackAction: TAction
      OnExecute = BackActionExecute
      OnUpdate = BackActionUpdate
    end
    object ForwardAction: TAction
      OnExecute = ForwardActionExecute
      OnUpdate = ForwardActionUpdate
    end
  end
end
