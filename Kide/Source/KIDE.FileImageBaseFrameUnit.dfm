inherited FileImageBaseFrame: TFileImageBaseFrame
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
      inline ImageViewerFrame: TImageViewerFrame
        Left = 0
        Top = 0
        Width = 601
        Height = 454
        Align = alClient
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        ExplicitLeft = 152
        ExplicitTop = 112
        ExplicitWidth = 601
        ExplicitHeight = 454
        inherited ViewerImage: TImage
          Width = 601
          Height = 429
          ExplicitWidth = 601
          ExplicitHeight = 429
        end
        inherited TopPanel: TPanel
          Width = 601
          ExplicitWidth = 601
        end
      end
    end
  end
end
