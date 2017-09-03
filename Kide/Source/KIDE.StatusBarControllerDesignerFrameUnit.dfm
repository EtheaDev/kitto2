inherited StatusBarControllerDesignerFrame: TStatusBarControllerDesignerFrame
  Height = 402
  HelpKeyword = 'StatusBar'
  ExplicitHeight = 402
  inherited ClientPanel: TPanel
    Height = 402
    ExplicitHeight = 402
    inherited DesignPanel: TPanel
      Height = 402
      ExplicitHeight = 402
      inherited ControllerGroupBox: TGroupBox
        Height = 402
        ExplicitHeight = 402
        object StatusBarControllerGroupBox: TGroupBox
          Left = 2
          Top = 72
          Width = 451
          Height = 56
          Align = alTop
          Caption = 'StatusBarController'
          TabOrder = 1
          DesignSize = (
            451
            56)
          object _Text: TLabeledEdit
            Left = 4
            Top = 32
            Width = 441
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            EditLabel.Width = 22
            EditLabel.Height = 13
            EditLabel.Caption = 'Text'
            TabOrder = 0
          end
        end
      end
    end
  end
end
