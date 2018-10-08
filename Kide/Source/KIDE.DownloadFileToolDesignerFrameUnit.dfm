inherited DownloadFileToolDesignerFrame: TDownloadFileToolDesignerFrame
  Width = 458
  Height = 405
  HelpKeyword = 'DownloadFile'
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
        ExplicitWidth = 450
        ExplicitHeight = 377
        inherited ToolGroupBox: TGroupBox
          Width = 454
          ExplicitWidth = 450
          DesignSize = (
            454
            60)
        end
        inherited DataToolGroupBox: TGroupBox
          Width = 454
          ExplicitLeft = 0
          ExplicitTop = 104
          ExplicitWidth = 450
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
        object DownloadToolGroupBox: TGroupBox
          Left = 2
          Top = 135
          Width = 454
          Height = 138
          Align = alTop
          Caption = 'DownloadFile'
          TabOrder = 2
          ExplicitLeft = 0
          ExplicitTop = 120
          ExplicitWidth = 450
          DesignSize = (
            454
            138)
          object _ClientFileName: TLabeledEdit
            Left = 6
            Top = 71
            Width = 244
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            EditLabel.Width = 70
            EditLabel.Height = 13
            EditLabel.Caption = 'ClientFileName'
            TabOrder = 1
          end
          object _FileName: TLabeledEdit
            Left = 6
            Top = 111
            Width = 440
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            EditLabel.Width = 43
            EditLabel.Height = 13
            EditLabel.Caption = 'FileName'
            TabOrder = 3
          end
          object _ConfirmationMessage: TLabeledEdit
            Left = 6
            Top = 32
            Width = 440
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            EditLabel.Width = 103
            EditLabel.Height = 13
            EditLabel.Caption = 'ConfirmationMessage'
            TabOrder = 0
          end
          object _ContentType: TLabeledEdit
            Left = 256
            Top = 71
            Width = 190
            Height = 21
            Anchors = [akTop, akRight]
            EditLabel.Width = 63
            EditLabel.Height = 13
            EditLabel.Caption = 'ContentType'
            TabOrder = 2
          end
        end
      end
    end
  end
end
