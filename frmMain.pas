unit frmMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Comctrls, ExtCtrls, ImgList,
  ShellAPI, clipbrd, Menus, FileCtrl, FolderDialog;

type
  TMainForm = class(TForm)
    fdFolder: TFolderDialog;
    pnlStatus: TPanel;
    ilImages: TImageList;
    lblCopyright: TLabel;
    lblWeb: TLabel;
    pcMain: TPageControl;
    tsTree: TTabSheet;
    tvShell: TTreeView;
    sdDialog: TSaveDialog;
    gbStatistics: TGroupBox;
    pnlFolderCount: TLabel;
    pnlFilesCount: TLabel;
    pnlSizeMB: TLabel;
    gbTools: TGroupBox;
    btnExpandAll: TButton;
    btnCollapseAll: TButton;
    btnClear: TButton;
    btnDoTextTree: TButton;
    btnSaveAs: TButton;
    gbOptions: TGroupBox;
    chbFoldersOnly: TCheckBox;
    chbCount: TCheckBox;
    chbShowInfoPanels: TCheckBox;
    gbButtons: TGroupBox;
    btnPickUpFolder: TButton;
    btnCancel: TButton;
    btnRefresh: TButton;
    chbAskBeforeRun: TCheckBox;
    btnExplore: TButton;
    btnOpen: TButton;
    btnFileDelete: TButton;
    btnFolderChangeName: TButton;
    btnFileExecute: TButton;

    function GetFullPathForSelectedNode(): String;
    function GetFileSize(FileName: String): Int64;
    function GetSizeWithProperPrefix(Size: Int64): String;
    function ListujKatalogi(tnRodzic: TTreeNode; strSciezka: string): Int64;
    function MakePolishEnding(Value: Integer; sText1, sText2, sText5, sAdd1, sAdd2, sAdd5: String): String;

    procedure DrawTree(Folder: String);
    procedure ChangeElementsEnableState(State: Boolean);

    procedure btnPickUpFolderClick(Sender: TObject);
    procedure btnExpandAllClick(Sender: TObject);
    procedure btnCollapseAllClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure chbCountClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure chbAskBeforeRunClick(Sender: TObject);
    procedure lblWebClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure chbFoldersOnlyClick(Sender: TObject);
    procedure btnDoTextTreeClick(Sender: TObject);
    procedure btnSaveAsClick(Sender: TObject);
    procedure btnFolderChangeNameClick(Sender: TObject);
    procedure btnFileExecuteClick(Sender: TObject);
    procedure chbShowInfoPanelsClick(Sender: TObject);
    procedure btnFileDeleteClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure tvShellChange(Sender: TObject; Node: TTreeNode);
    procedure btnExploreClick(Sender: TObject);
  private
    { Private declarations }
  public
    fiCount, foCount, siCount: Int64;
  end;

var
  MainForm: TMainForm;
  GlobalCancel: Boolean;

implementation

{$R *.DFM}
{$R WinXP.res}

function TMainForm.GetFullPathForSelectedNode(): String;
var
        tnTemp: TTreeNode;
        sTemp, sPath: String;
begin
        Result := '';

        tnTemp := tvShell.Selected;
        sPath := tnTemp.Text;

        if Pos('(', sPath) > 0 then sPath := Copy(sPath, 1, Pos('(', sPath) - 2);
        if Pos('[', sPath) > 0 then sPath := Copy(sPath, 2, Pos(']', sPath) - 2);

        while tnTemp.Parent <> nil do
        begin
                tnTemp := tnTemp.Parent;
                sTemp := tnTemp.Text;

                if Pos('(', sTemp) > 0 then sTemp := Copy(sTemp, 1, Pos('(', sTemp) - 2);
                if Pos('[', sTemp) > 0 then sTemp := Copy(sTemp, 2, Pos(']', sTemp) - 2);

                sPath := sTemp + '\' + sPath;
        end;

        if Pos('Mój komputer\', sPath) > 0 then sPath := Copy(sPath, 14, Length(sPath));

        Result := sPath;
end;

function TMainForm.GetFileSize(FileName: String): Int64;
var
        SearchRec: TSearchRec;
begin
        if FindFirst(ExpandFileName(FileName), faAnyFile, SearchRec) = 0 then
        begin
                Result := SearchRec.Size;
                SysUtils.FindClose(SearchRec);
        end
        else Result := 0;
end;

function TMainForm.GetSizeWithProperPrefix(Size: Int64): String;
begin
        //Size := Abs(Size);
        Result := IntToStr(Size) + ' B';

        if Size > 1024 then Result := IntToStr(Round(Size / 1024)) + ' kB';
        if Size > 1048576 then Result := FloatToStrF(Size / 1048576, ffFixed, 7, 2) + ' MB';
        if Size > 1073741824 then Result := FloatToStrF(Size / 1073741824, ffFixed, 7, 2) + ' GB';
end;

function TMainForm.ListujKatalogi(tnRodzic: TTreeNode; strSciezka: string): Int64;
var
        sr: TSearchRec;
        iSize, iWynik: Integer;
        tnDziecko: TTreeNode;
        sSize: String;
        DirSize, TotalSize: Int64;
begin
        pnlStatus.Caption := ' Dodawanie: ' + strSciezka;
        Application.ProcessMessages;

        TotalSize := 0;

        strSciezka := IncludeTrailingBackslash(strSciezka);
        iWynik := FindFirst(strSciezka + '*.*', faAnyFile, sr);

        while iWynik = 0 do
        begin
                if GlobalCancel = True then
                begin
                        FindClose(sr);

                        if not chbFoldersOnly.Checked then
                        begin
                                pnlFilesCount.Caption := MakePolishEnding(fiCount, 'plik', 'pliki', 'plików', '', '', '');
                                pnlFolderCount.Caption := MakePolishEnding(foCount, 'folder', 'foldery', 'folderów', '', '', '');
                                pnlSizeMB.Caption := IntToStr(Round(siCount / 1048576)) + ' MB (' + FloatToStrF(siCount / 1073741824, ffFixed, 7, 2) + ' GB)';
                        end;

                        Result := TotalSize;
                        exit;
                end;

                if ((sr.Attr and faDirectory) = faDirectory) and (sr.Name <> '.') and (sr.Name <> '..') then
                begin
                        tnDziecko := tvShell.Items.AddChild(tnRodzic, '[' + sr.Name + ']');
                        tnDziecko.ImageIndex := 0;
                        tnDziecko.SelectedIndex := 0;
                        Inc(foCount);

                        DirSize := ListujKatalogi(tnDziecko, strSciezka + sr.Name + '\');
                        Inc(TotalSize, DirSize);

                        if chbCount.Checked then
                        begin
                                sSize := GetSizeWithProperPrefix(DirSize);
                                tnDziecko.Text := tnDziecko.Text + ' (' + sSize + ')'
                        end;
                end
                else
                begin
                        if (sr.Name <> '.') and (sr.Name <> '..') and (chbFoldersOnly.Checked <> True) then
                        begin
                                iSize := GetFileSize(strSciezka + sr.Name);
                                sSize := GetSizeWithProperPrefix(iSize);

                                Inc(TotalSize, iSize);

                                Inc(fiCount);
                                Inc(siCount, iSize);

                                if chbCount.Checked then
                                        tnDziecko := tvShell.Items.AddChild(tnRodzic, sr.Name + ' (' + sSize + ')')
                                else
                                        tnDziecko := tvShell.Items.AddChild(tnRodzic, sr.Name);

                                tnDziecko.ImageIndex := 1;
                                tnDziecko.SelectedIndex := 1;
                        end;
                end;

                iWynik := FindNext(sr);
        end;

        FindClose(sr);

        if not chbFoldersOnly.Checked then
        begin
                pnlFilesCount.Caption := MakePolishEnding(fiCount, 'plik', 'pliki', 'plików', '', '', '');
                pnlFolderCount.Caption := MakePolishEnding(foCount, 'folder', 'foldery', 'folderów', '', '', '');
                pnlSizeMB.Caption := IntToStr(Round(siCount / 1048576)) + ' MB (' + FloatToStrF(siCount / 1073741824, ffFixed, 7, 2) + ' GB)';

                Application.ProcessMessages;
        end;

        Result := TotalSize;
end;

procedure TMainForm.DrawTree(Folder: String);
var
        tnRoot, tnTemp: TTreeNode;
        Start: DWORD;
        iDrvType: Integer;
        cDrive: Char;
        sSize, sETBs: String;
        TotalSize, DirSize: Int64;
begin
        Screen.Cursor := crHourglass;
        Start := GetTickCount();

        ChangeElementsEnableState(False);
        btnCancel.Enabled := True;

        btnClearClick(self);
        tvShell.Items.BeginUpdate;

        TotalSize := 0;

        if Folder = '' then
        begin
                tnRoot := tvShell.Items.AddChild(nil, '[Mój komputer]');
                tnRoot.ImageIndex := 7;
                tnRoot.SelectedIndex := 7;

                for cDrive := 'a' to 'z' do
                begin
                        iDrvType := GetDriveType(PChar(cDrive + ':\'));
                        if (iDrvType <> 0) and (iDrvType <> 1) then
                        begin
                                tnTemp := tvShell.Items.AddChild(tnRoot, UpperCase(cDrive) + ':');
                                tnTemp.ImageIndex := iDrvType;
                                tnTemp.SelectedIndex := iDrvType;

                                DirSize := ListujKatalogi(tnTemp, UpperCase(cDrive) + ':\');
                                Inc(TotalSize, DirSize);

                                if chbCount.Checked then
                                begin
                                        sSize := GetSizeWithProperPrefix(DirSize);
                                        tnTemp.Text := tnTemp.Text + ' (' + sSize + ')'
                                end;
                        end;
                end;

                if chbCount.Checked then
                begin
                        sSize := GetSizeWithProperPrefix(TotalSize);
                        tnRoot.Text := tnRoot.Text + ' (' + sSize + ')'
                end;
        end
        else
        begin
                sETBs := ExcludeTrailingBackslash(Folder);
                tnRoot := tvShell.Items.AddChild(nil, '[' + sETBs + ']');

                if sETBs = ExtractFileDrive(Folder) then
                begin
                        tnRoot.ImageIndex := GetDriveType(PChar(sETBs));
                        tnRoot.SelectedIndex := GetDriveType(PChar(sETBs));
                end
                else
                begin
                        tnRoot.ImageIndex := 0;
                        tnRoot.SelectedIndex := 0;
                end;

                DirSize := ListujKatalogi(tnRoot, Folder);

                if chbCount.Checked then
                begin
                        sSize := GetSizeWithProperPrefix(DirSize);
                        tnRoot.Text := tnRoot.Text + ' (' + sSize + ')'
                end;
        end;

        tvShell.AlphaSort();
        tnRoot.Expand(False);
        tvShell.Items.EndUpdate;

        ChangeElementsEnableState(True);
        btnCancel.Enabled := False;

        pnlStatus.Caption := ' Gotowy. Czas operacji: ' + IntToStr(GetTickCount() - Start) + ' ms.';
        Screen.Cursor := crDefault;
end;

procedure TMainForm.btnPickUpFolderClick(Sender: TObject);
begin
        if fdFolder.Directory = '' then fdFolder.Directory := ExtractFilePath(Application.ExeName);
        if not fdFolder.Execute then exit;

        GlobalCancel := False;
        DrawTree(fdFolder.Directory);
end;

procedure TMainForm.btnExpandAllClick(Sender: TObject);
var
        Start: DWORD;
begin
        if tvShell.Items.Count < 2 then exit;

        Screen.Cursor := crHourglass;
        Start := GetTickCount();

        ChangeElementsEnableState(False);
        
        pnlStatus.Caption := ' Rozwijanie wszystkich ga³êzi drzewa...';
        Application.ProcessMessages;

        tvShell.FullExpand;

        ChangeElementsEnableState(True);

        pnlStatus.Caption := ' Gotowy. Czas operacji: ' + IntToStr(GetTickCount() - Start) + ' ms.';
        Screen.Cursor := crDefault;
end;

procedure TMainForm.btnCollapseAllClick(Sender: TObject);
var
        Start: DWORD;
begin
        if tvShell.Items.Count < 2 then exit;
        
        Screen.Cursor := crHourglass;
        Start := GetTickCount();
        
        pnlStatus.Caption := ' Zwijanie wszystkich ga³êzi drzewa...';
        Application.ProcessMessages;

        ChangeElementsEnableState(False);

        tvShell.FullCollapse;

        tvShell.Items[0].Expand(False);

        ChangeElementsEnableState(True);

        pnlStatus.Caption := ' Gotowy. Czas operacji: ' + IntToStr(GetTickCount() - Start) + ' ms.';
        Screen.Cursor := crDefault;
end;

procedure TMainForm.btnClearClick(Sender: TObject);
begin
        tvShell.Items.Clear;
        pnlStatus.Caption := ' Gotowy.';

        fiCount := 0;
        foCount := 0;
        siCount := 0;
        if not chbFoldersOnly.Checked then
        begin
                pnlFilesCount.Caption := '0 plików';
                pnlFolderCount.Caption := '0 folderów';
                pnlSizeMB.Caption := IntToStr(Round(siCount / 1048576)) + ' MB (' + FloatToStrF(siCount / 1073741824, ffFixed, 7, 2) + ' GB)';
        end;

        btnDoTextTree.Enabled := False;
        btnSaveAs.Enabled := False;
        btnExpandAll.Enabled := False;
        btnCollapseAll.Enabled := False;
end;

procedure TMainForm.chbCountClick(Sender: TObject);
begin
        gbStatistics.Visible := chbCount.Checked;

        if (gbStatistics.Visible) and (not chbFoldersOnly.Checked) then
        begin
                pnlFilesCount.Caption := '0 plików';
                pnlFolderCount.Caption := '0 folderów';
                pnlSizeMB.Caption := IntToStr(Round(siCount / 1048576)) + ' MB (' + FloatToStrF(siCount / 1073741824, ffFixed, 7, 2) + ' GB)';
        end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
        GlobalCancel := False;
end;

procedure TMainForm.btnCancelClick(Sender: TObject);
begin
        GlobalCancel := True;
end;

procedure TMainForm.chbAskBeforeRunClick(Sender: TObject);
begin
        if not chbAskBeforeRun.Checked then if Application.MessageBox('Gdy pole to jest odznaczone, przypadkowe naciœniêcie klawisza Delete spowoduje usuniêcie pliku bez potwierdzenia!'+chr(13)+'(Uwaga! Pliki s¹ usuwane BEZ wykorzystania Kosza - jest to operacja NIEODWRACALNA!)'+chr(13)+''+chr(13)+'Czy na pewno wy³¹czyæ ostrze¿enia?','Ostrze¿enie!',MB_YESNO+MB_ICONWARNING+MB_DEFBUTTON2) = ID_NO then chbAskBeforeRun.Checked := True;
end;

procedure TMainForm.lblWebClick(Sender: TObject);
begin
        ShellExecute(Handle,'open','http://www.trejderowski.pl/','','',SW_SHOW);
end;

procedure TMainForm.btnRefreshClick(Sender: TObject);
begin
        GlobalCancel := False;
        DrawTree(fdFolder.Directory);
end;

procedure TMainForm.chbFoldersOnlyClick(Sender: TObject);
begin
        if chbFoldersOnly.Checked then
        begin
                pnlFilesCount.Enabled := False;
                pnlSizeMB.Enabled := False;
                pnlFilesCount.Caption := '--';
                pnlSizeMB.Caption := '--';
        end
        else
        begin
                pnlFilesCount.Enabled := True;
                pnlSizeMB.Enabled := True;
                pnlFilesCount.Caption := IntToStr(fiCount);
                pnlSizeMB.Caption := IntToStr(Round(siCount / 1048576)) + ' MB (' + FloatToStrF(siCount / 1073741824, ffFixed, 7, 2) + ' GB)';
        end;
end;

procedure TMainForm.btnDoTextTreeClick(Sender: TObject);
var
        Start: DWORD;
        clp: TClipboard;
        slTemp: TStringList;
begin
        if tvShell.Items.Count < 2 then exit;

        Screen.Cursor := crHourglass;
        Start := GetTickCount();

        pnlStatus.Caption := ' Kopiowanie zawartoœci drzewa do Schowka...';
        Application.ProcessMessages;

        ChangeElementsEnableState(False);

        try
                tvShell.SaveToFile('temp.txt');

                slTemp := TStringList.Create;
                clp := TClipboard.Create;

                slTemp.LoadFromFile('temp.txt');
                clp.AsText := slTemp.Text;

                clp.Free;
                slTemp.Free;

                DeleteFile('temp.txt');
        except
                Application.MessageBox('Aby wykonaæ t¹ operacjê, program musi zapisaæ (a nastêpnie usun¹æ) plik tymczasowy. Jednak¿e, podczas próby zapisu pliku tymczasowego wyst¹pi³ b³¹d i operacja zosta³a przerwana.'+chr(13)+''+chr(13)+'SprawdŸ, czy dysk (lub folder), z którego zosta³ uruchomiony program FaFTB nie jest chroniony przed zapisem i ewentualnie spróbuj ponownie.','B³¹d!',MB_OK+MB_ICONWARNING+MB_DEFBUTTON1);
        end;

        ChangeElementsEnableState(True);

        pnlStatus.Caption := ' Gotowy. Czas operacji: ' + IntToStr(GetTickCount() - Start) + ' ms.';
        Screen.Cursor := crDefault;
end;

procedure TMainForm.btnSaveAsClick(Sender: TObject);
var
        Start: DWORD;
begin
        if not sdDialog.Execute then exit;

        Screen.Cursor := crHourglass;
        Start := GetTickCount();

        pnlStatus.Caption := ' Zapisywanie zawartoœci drzewa do pliku tekstowego...';
        Application.ProcessMessages;

        ChangeElementsEnableState(False);

        try
                tvShell.SaveToFile(sdDialog.FileName);
        except
                Application.MessageBox('Wyst¹pi³ b³¹d przy próbie zapisu zawartoœci drzewa do pliku tekstowego. Byæ mo¿e docelowy napêd jest zabezpieczony przed zapisem?','B³¹d!',MB_OK+MB_ICONWARNING+MB_DEFBUTTON1);
        end;

        ChangeElementsEnableState(True);

        pnlStatus.Caption := ' Gotowy. Czas operacji: ' + IntToStr(GetTickCount() - Start) + ' ms.';
        Screen.Cursor := crDefault;
end;

procedure TMainForm.btnFolderChangeNameClick(Sender: TObject);
var
        sSize, sPath, sSource, sDest: String;
        iSize: Integer;
begin
        if tvShell.Selected = nil then exit;

        sPath := ExtractFilePath(GetFullPathForSelectedNode());
        sSource := ExtractFileName(GetFullPathForSelectedNode());
        sDest := InputBox('Zmiana nazwy', 'Podaj now¹ nazwê dla wskazanego pliku:', sSource);

        if sDest = sSource then exit;

        if chbAskBeforeRun.Checked then if Application.MessageBox(PChar('Obecna nazwa: ' + sSource + chr(13) + 'Nowa nazwa: ' + sDest + chr(13) + chr(13) + 'Folder: ' + sPath + chr(13) + chr(13) + 'Czy na pewno zmieniæ nazwê pliku?'),'Zmiana nazwy pliku...',MB_YESNO+MB_ICONQUESTION+MB_DEFBUTTON2) = ID_NO then exit;

        if RenameFile(sPath + sSource, sPath + sDest) then
        begin
                iSize := GetFileSize(sPath + sDest);
                sSize := GetSizeWithProperPrefix(iSize);

                tvShell.Selected.Text := sDest + ' (' + sSize + ')';
        end
        else Application.MessageBox('Zmiana nazwy pliku nie powiod³a siê!','B³¹d...',MB_OK+MB_ICONWARNING+MB_DEFBUTTON1);

        tvShell.SetFocus;
end;

procedure TMainForm.btnFileExecuteClick(Sender: TObject);
var
        sPath: String;
begin
        if tvShell.Selected = nil then exit;

        sPath := GetFullPathForSelectedNode();
        if not FileExists(sPath) then exit;

        if chbAskBeforeRun.Checked then
        begin
                if ExtractFileExt(sPath) = '.exe' then
                begin
                        if Application.MessageBox(PChar('Czy uruchomiæ program:'+chr(13)+sPath+chr(13)+chr(13)+'Niektóre pliki wykonywalne mog¹ zawieraæ wirusy lub stanowiæ inne potencjalne zagro¿enie dla twojego komputera!'),'Uruchomienie pliku...',MB_YESNO+MB_ICONQUESTION+MB_DEFBUTTON2) = ID_NO then exit;

                        ShellExecute(Handle, 'open', PChar(sPath), '', '', SW_SHOW);
                end
                else
                begin
                        if Application.MessageBox(PChar('Czy uruchomiæ plik:'+chr(13)+sPath+chr(13)+chr(13)+'Uruchomiony zostanie program skojarzony w systemie z typem wybranego pliku.'),'Uruchomienie pliku...',MB_YESNO+MB_ICONQUESTION+MB_DEFBUTTON2) = ID_NO then exit;

                        ShellExecute(Handle, 'open', PChar(sPath), '', '', SW_SHOW);
                end;
        end
        else ShellExecute(Handle, 'open', PChar(sPath), '', '', SW_SHOW);

        tvShell.SetFocus();
end;

procedure TMainForm.chbShowInfoPanelsClick(Sender: TObject);
begin
        gbTools.Visible := chbShowInfoPanels.Checked;
end;

function TMainForm.MakePolishEnding(Value: Integer; sText1, sText2, sText5, sAdd1, sAdd2, sAdd5: String): String;
var
        iResult, iLast, iLast_Two: Integer;
        sBeginAdd, sBaseText, sValue: String;
begin
     //Parametry wejœciowe
     //Value - wartoœæ, na podstawie której jest tworzone polskie dope³nienie.
     //sText1 - tekst dla przypadku pierwszego (patrz poni¿ej), np. "sekunda" ("1 sekunda", st¹d nazwa zmiennej).
     //sText2 - tekst dla przypadku drugiego (patrz poni¿ej), np. "sekundy" ("2 sekundy", st¹d nazwa zmiennej).
     //sText5 - tekst dla przypadku trzeciego (patrz poni¿ej), np. "sekund" ("5 sekund", st¹d nazwa zmiennej).
     //sAdd1 - przedrostek dla przypadku pierwszego (patrz poni¿ej), np. "pozosta³a" ("pozosta³a 1 sekunda", st¹d nazwa zmiennej).
     //sAdd2 - przedrostek dla przypadku drugiego (patrz poni¿ej), np. "pozosta³y" ("pozosta³y 2 sekundy", st¹d nazwa zmiennej).
     //sAdd5 - przedrostek dla przypadku trzeciego (patrz poni¿ej), np. "pozosta³o" ("pozosta³o 5 sekund", st¹d nazwa zmiennej).
     //
     //Funkcja zwraca wartoœæ 1-3 w zale¿noœci, od tego jakie dope³nienie powinno byæ:
     //1 - 'a': tylko dla jednoœci, np. "1 sekunda", "1 minuta", "1 godzina".
     //2 - 'y': dla wartoœci jednoœci 2-4, np. "2 sekundy", "22 minuty", "194 godziny" (bez liczb 11-19),
     //3 - '[puste]': dla jednoœci (np. "21 sekund"), liczb 11-19 (np. "14 minut", "217 godzin") i wszystkich pozosta³ych.

     iResult := 3; //Domyœlnie 3, bo najwiêcej liczb spe³nia trzeci warunek

     sValue := IntToStr(Value);

     iLast := StrToIntDef(Copy(sValue, Length(sValue), 1), 0); //Ostatnia cyfra
     iLast_Two := StrToIntDef(Copy(sValue, Length(sValue) - 1, 2), 0); //Ostatnie dwie cyfry

     if ((iLast > 1) and (iLast < 5)) then iResult := 2; //Liczby z 2-4 na pozycji jednoœci - drugi warunek

     if ((iLast_Two > 10) and (iLast_Two < 20)) then iResult := 3; //Wymuszenie warunku 3 dla liczb maj¹cych 11-19 na pozycji dziesi¹tek

     if (Value = 1) then iResult := 1; //Jedyny taki przypadek - warunek pierwszy spe³nia tylko cyfra 1.

     sBeginAdd := '';

     if (iResult = 1) then
     begin
	  sBaseText := sText1;
	  if (sAdd1 <> '') then sBeginAdd := sAdd1 + ' ';
     end;
     
     if (iResult = 2) then
     begin
	  sBaseText := sText2;
	  if (sAdd2 <> '') then sBeginAdd := sAdd2 + ' ';
     end;
     
     if (iResult = 3) then
     begin
	  sBaseText := sText5;
	  if (sAdd5 <> '') then sBeginAdd := sAdd5 + ' ';
     end;
	
     Result := sBeginAdd + IntToStr(Value) + ' ' + sBaseText;
end;

procedure TMainForm.ChangeElementsEnableState(State: Boolean);
begin
        btnPickUpFolder.Enabled := State;
        btnExpandAll.Enabled := State;
        btnCollapseAll.Enabled := State;
        btnClear.Enabled := State;
        chbCount.Enabled := State;
        chbShowInfoPanels.Enabled := State;
        chbAskBeforeRun.Enabled := State;
        btnDoTextTree.Enabled := State;
        btnSaveAs.Enabled := State;
        btnRefresh.Enabled := State;
        chbFoldersOnly.Enabled := State;
end;

procedure TMainForm.btnFileDeleteClick(Sender: TObject);
var
        sFile: String;
begin
        if tvShell.Selected = nil then exit;

        sFile := GetFullPathForSelectedNode();

        if chbAskBeforeRun.Checked then if Application.MessageBox(PChar('Uwaga! Poni¿szy plik zostanie TRWALE usuniêty!' + chr(13) + chr(13) + sFile + chr(13) + chr(13) + 'Czy na pewno usun¹æ ten plik?' + chr(13) + '(Uwaga! Pliki s¹ usuwane BEZ wykorzystania Kosza - jest to operacja NIEODWRACALNA!)'),'Usuwanie pliku...',MB_YESNO+MB_ICONQUESTION+MB_DEFBUTTON2) = ID_NO then exit;

        if not DeleteFile(sFile) then
                Application.MessageBox('Usuwanie pliku nie powiod³o siê!','B³¹d...',MB_OK+MB_ICONWARNING+MB_DEFBUTTON1)
        else
                tvShell.Selected.Delete;

        tvShell.SetFocus();
end;

procedure TMainForm.btnOpenClick(Sender: TObject);
var
        sPath: String;
begin
        if tvShell.Selected = nil then exit;

        if tvShell.Selected.ImageIndex <> 1 then
                sPath := ExtractFilePath(IncludeTrailingBackslash(GetFullPathForSelectedNode()))
        else
                sPath := ExtractFilePath(GetFullPathForSelectedNode());

        if not DirectoryExists(sPath) then exit;

        ShellExecute(Handle, 'open', PChar(sPath), '', '', SW_SHOW);

        tvShell.SetFocus();
end;

procedure TMainForm.tvShellChange(Sender: TObject; Node: TTreeNode);
begin
        if tvShell.Selected = nil then
        begin
                btnFileExecute.Enabled := False;
                btnFolderChangeName.Enabled := False;
                btnFileDelete.Enabled := False;
                btnOpen.Enabled := False;
                btnExplore.Enabled := False;
                exit;
        end
        else
        begin

                btnFileExecute.Enabled := (tvShell.Selected.ImageIndex = 1);
                btnFolderChangeName.Enabled := (tvShell.Selected.ImageIndex = 1);
                btnFileDelete.Enabled := (tvShell.Selected.ImageIndex = 1);

                //btnOpen.Enabled := (tvShell.Selected.ImageIndex <> 1);
                btnOpen.Enabled := True;
                //btnExplore.Enabled := (tvShell.Selected.ImageIndex <> 1);
                btnExplore.Enabled := True;
        end;
end;

procedure TMainForm.btnExploreClick(Sender: TObject);
var
        sPath: String;
begin
        if tvShell.Selected = nil then exit;

        if tvShell.Selected.ImageIndex <> 1 then
                sPath := ExtractFilePath(IncludeTrailingBackslash(GetFullPathForSelectedNode()))
        else
                sPath := ExtractFilePath(GetFullPathForSelectedNode());
                
        if not DirectoryExists(sPath) then exit;

        ShellExecute(Handle, 'explore', PChar(sPath), '', '', SW_SHOW);

        tvShell.SetFocus();
end;

end.
