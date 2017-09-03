inherited FileEditorBaseFrame: TFileEditorBaseFrame
  Width = 601
  Height = 454
  ExplicitWidth = 601
  ExplicitHeight = 454
  object ClientPanel: TPanel
    Left = 0
    Top = 0
    Width = 601
    Height = 454
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object EditPanel: TPanel
      Left = 0
      Top = 0
      Width = 601
      Height = 454
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      inline CodeEditorFrame: TCodeEditorFrame
        Left = 0
        Top = 0
        Width = 601
        Height = 454
        Align = alClient
        DoubleBuffered = False
        ParentDoubleBuffered = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        ExplicitWidth = 601
        ExplicitHeight = 454
        inherited ErrorLabel: TLabel
          Top = 438
          Width = 601
          ExplicitTop = 85
          ExplicitWidth = 601
        end
        inherited ToolBar: TToolBar
          Width = 601
          ExplicitWidth = 601
          inherited ApplyActionToolButton: TToolButton
            Visible = False
          end
          inherited CancelActionToolButton: TToolButton
            Visible = False
          end
          inherited Sep1ToolButton: TToolButton
            Visible = False
          end
        end
      end
    end
  end
end
