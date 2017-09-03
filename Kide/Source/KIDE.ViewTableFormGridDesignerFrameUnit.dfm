inherited ViewTableFormGridDesignerFrame: TViewTableFormGridDesignerFrame
  Width = 592
  ExplicitWidth = 592
  inherited ClientPanel: TPanel
    Width = 592
    ExplicitWidth = 592
    inherited DesignPanel: TPanel
      Width = 592
      ExplicitWidth = 592
      object FormGridGroupBox: TGroupBox
        Left = 0
        Top = 0
        Width = 592
        Height = 57
        Align = alTop
        Caption = 'Form/Grid'
        TabOrder = 0
        object ViewNameSpeedButton: TSpeedButton
          Left = 202
          Top = 28
          Width = 24
          Height = 23
          Action = FileOpenLayoutAction
        end
        object _Layout: TLabeledEdit
          Left = 7
          Top = 29
          Width = 195
          Height = 21
          EditLabel.Width = 33
          EditLabel.Height = 13
          EditLabel.Caption = 'Layout'
          TabOrder = 0
        end
      end
    end
  end
  inherited ActionList: TActionList
    object FileOpenLayoutAction: TFileOpen
      Category = 'File'
      Dialog.DefaultExt = '*.yaml'
      Dialog.Filter = 'Kitto Layout files|*.yaml'
      Hint = 'Open|Opens an existing Layout file'
      ImageIndex = 0
      ShortCut = 16463
      BeforeExecute = FileOpenLayoutActionBeforeExecute
      OnAccept = FileOpenLayoutActionAccept
    end
  end
end
