inherited TreeDesignerFrame: TTreeDesignerFrame
  Width = 394
  Height = 448
  ParentShowHint = False
  ExplicitWidth = 394
  ExplicitHeight = 448
  inherited ClientPanel: TPanel
    Width = 373
    Height = 448
    TabOrder = 1
    ExplicitWidth = 373
    ExplicitHeight = 448
    inherited DesignPanel: TPanel
      Top = 17
      Width = 373
      Height = 391
      TabOrder = 1
      ExplicitTop = 17
      ExplicitWidth = 373
      ExplicitHeight = 391
      object ToolBar: TToolBar
        Left = 0
        Top = 0
        Width = 373
        Height = 26
        AutoSize = True
        ButtonHeight = 26
        ButtonWidth = 26
        Images = MainDataModule.Images
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        object ApplyActionToolButton: TToolButton
          Left = 0
          Top = 0
          Action = ApplyAction
        end
        object CancelActionToolButton: TToolButton
          Left = 26
          Top = 0
          Action = CancelAction
        end
      end
    end
    object PathPanel: TStaticText
      Left = 0
      Top = 0
      Width = 373
      Height = 17
      Hint = 'Path of current Node'
      Align = alTop
      Caption = 'Path'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      ExplicitWidth = 26
    end
    inline CodeEditorFrame: TCodeEditorFrame
      Left = 0
      Top = 408
      Width = 373
      Height = 40
      Align = alBottom
      DoubleBuffered = False
      ParentDoubleBuffered = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      ExplicitTop = 408
      ExplicitWidth = 373
      ExplicitHeight = 40
      inherited ErrorLabel: TLabel
        Top = 24
        Width = 373
        ExplicitTop = 286
        ExplicitWidth = 373
      end
      inherited ToolBar: TToolBar
        Width = 373
        ExplicitWidth = 373
      end
    end
  end
  object TabSet: TTabSet [1]
    Left = 373
    Top = 0
    Width = 21
    Height = 448
    Align = alRight
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ShrinkToFit = True
    SoftTop = True
    Tabs.Strings = (
      'Design'
      'Source')
    TabIndex = 0
    TabPosition = tpRight
    OnChange = TabSetChange
  end
  inherited ActionList: TActionList
    object ApplyAction: TAction
      Caption = 'Apply'
      Enabled = False
      Hint = 'Apply Edits'
      ImageIndex = 32
      ShortCut = 16467
      OnExecute = ApplyActionExecute
      OnUpdate = ApplyActionUpdate
    end
    object CancelAction: TAction
      Caption = 'Cancel'
      Enabled = False
      Hint = 'Cancel Edits'
      ImageIndex = 33
      OnExecute = CancelActionExecute
      OnUpdate = CancelActionUpdate
    end
  end
end
