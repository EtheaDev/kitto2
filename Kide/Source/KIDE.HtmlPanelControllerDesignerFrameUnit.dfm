inherited HtmlPanelControllerDesignerFrame: THtmlPanelControllerDesignerFrame
  Height = 402
  HelpKeyword = 'HTMLPanel'
  ExplicitHeight = 402
  inherited ClientPanel: TPanel
    Height = 402
    ExplicitHeight = 402
    inherited DesignPanel: TPanel
      Height = 402
      ExplicitHeight = 402
      inherited ControllerGroupBox: TGroupBox
        ExplicitHeight = 374
        object HtmlPanelControllerGroupBox: TGroupBox
          Left = 0
          Top = 140
          Width = 447
          Height = 234
          Align = alClient
          Caption = 'HtmlPanelController'
          TabOrder = 1
          ExplicitTop = 0
          ExplicitWidth = 455
          ExplicitHeight = 402
          DesignSize = (
            451
            328)
          object HtmlLabel: TLabel
            Left = 6
            Top = 55
            Width = 21
            Height = 13
            Caption = 'Html'
          end
          object _FileName: TLabeledEdit
            Left = 3
            Top = 28
            Width = 446
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            EditLabel.Width = 43
            EditLabel.Height = 13
            EditLabel.Caption = 'FileName'
            TabOrder = 0
            ExplicitWidth = 438
          end
          object HtmlPanel: TPanel
            Left = 6
            Top = 72
            Width = 443
            Height = 342
            Anchors = [akLeft, akTop, akRight, akBottom]
            BevelOuter = bvLowered
            TabOrder = 1
            ExplicitWidth = 435
            ExplicitHeight = 154
          end
        end
      end
    end
  end
end
