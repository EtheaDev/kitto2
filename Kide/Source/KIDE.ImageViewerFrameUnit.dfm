inherited ImageViewerFrame: TImageViewerFrame
  Width = 398
  Height = 298
  ExplicitWidth = 398
  ExplicitHeight = 298
  object ViewerImage: TImage
    Left = 0
    Top = 25
    Width = 398
    Height = 273
    Align = alClient
    Proportional = True
    ExplicitLeft = 112
    ExplicitTop = 72
    ExplicitWidth = 105
    ExplicitHeight = 105
  end
  object TopPanel: TPanel
    Left = 0
    Top = 0
    Width = 398
    Height = 25
    Align = alTop
    BevelOuter = bvLowered
    TabOrder = 0
    object StretchCheckBox: TCheckBox
      Left = 7
      Top = 5
      Width = 82
      Height = 17
      Caption = 'Stretch'
      TabOrder = 0
      OnClick = StretchCheckBoxClick
    end
    object ProportionalCheckBox: TCheckBox
      Left = 88
      Top = 5
      Width = 82
      Height = 17
      Caption = 'Proportional'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = ProportionalCheckBoxClick
    end
  end
end
