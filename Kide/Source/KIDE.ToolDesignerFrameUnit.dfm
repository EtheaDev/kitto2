inherited ToolDesignerFrame: TToolDesignerFrame
  Width = 452
  HelpKeyword = 'HowToAction'
  ExplicitWidth = 452
  inherited ClientPanel: TPanel
    Width = 452
    ExplicitWidth = 452
    inherited DesignPanel: TPanel
      Width = 452
      ExplicitWidth = 452
      inherited ControllerGroupBox: TGroupBox
        Width = 452
        ExplicitWidth = 452
        object ToolGroupBox: TGroupBox
          Left = 2
          Top = 15
          Width = 448
          Height = 60
          Align = alTop
          Caption = 'Tool'
          TabOrder = 0
          DesignSize = (
            448
            60)
          object _Title: TLabeledEdit
            Left = 4
            Top = 31
            Width = 440
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            EditLabel.Width = 20
            EditLabel.Height = 13
            EditLabel.Caption = 'Title'
            TabOrder = 0
          end
        end
      end
    end
  end
end
