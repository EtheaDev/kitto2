inherited UploadFileToolDesignerFrame: TUploadFileToolDesignerFrame
  Width = 458
  Height = 405
  HelpKeyword = 'UploadFile'
  ExplicitWidth = 458
  ExplicitHeight = 405
  inherited ClientPanel: TPanel
    Width = 458
    Height = 405
    ExplicitWidth = 458
    ExplicitHeight = 405
    inherited DesignPanel: TPanel
      Width = 458
      Height = 405
      ExplicitWidth = 458
      ExplicitHeight = 405
      inherited ControllerGroupBox: TGroupBox
        Width = 458
        Height = 405
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 458
        ExplicitHeight = 405
        inherited ToolGroupBox: TGroupBox
          Width = 454
          ExplicitWidth = 454
          DesignSize = (
            454
            60)
        end
        inherited DataToolGroupBox: TGroupBox
          Width = 454
          ExplicitLeft = 2
          ExplicitTop = 75
          ExplicitWidth = 454
          DesignSize = (
            454
            60)
          inherited _AutoRefresh: TComboBox
            Width = 322
            ExplicitWidth = 322
          end
          inherited _RequireSelection: TCheckBox
            Left = 334
            ExplicitLeft = 334
          end
        end
        object UploadToolGroupBox: TGroupBox
          Left = 2
          Top = 135
          Width = 454
          Height = 100
          Align = alTop
          Caption = 'UploadFile'
          TabOrder = 2
          DesignSize = (
            454
            100)
          object _Path: TLabeledEdit
            Left = 6
            Top = 32
            Width = 440
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            EditLabel.Width = 22
            EditLabel.Height = 13
            EditLabel.Caption = 'Path'
            TabOrder = 0
          end
          object _ContentType: TLabeledEdit
            Left = 256
            Top = 71
            Width = 190
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            EditLabel.Width = 63
            EditLabel.Height = 13
            EditLabel.Caption = 'ContentType'
            TabOrder = 3
          end
          object _AcceptedWildcards: TLabeledEdit
            Left = 6
            Top = 71
            Width = 91
            Height = 21
            EditLabel.Width = 91
            EditLabel.Height = 13
            EditLabel.Caption = 'AcceptedWildcards'
            TabOrder = 1
          end
          object _MaxUploadSize: TLabeledEdit
            Left = 103
            Top = 71
            Width = 147
            Height = 21
            EditLabel.Width = 72
            EditLabel.Height = 13
            EditLabel.Caption = 'MaxUploadSize'
            TabOrder = 2
          end
        end
      end
    end
  end
end
