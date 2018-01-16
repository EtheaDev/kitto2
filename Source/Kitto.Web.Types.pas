unit Kitto.Web.Types;

interface

function GetFileMimeType(const AFileName: string; const ADefaultType: string = 'application/octet-stream'): string;

implementation

uses
  SysUtils
  ;

type
  TMIMEType = record
    Ext: string;
    MimeType: string;
  end;

const
  MIME_TYPES: array [1 .. 176] of TMIMEType = (
  (Ext: '.gif'; MimeType: 'image/gif'), (Ext: '.jpg'; MimeType: 'image/jpeg'), (Ext: '.jpeg'; MimeType: 'image/jpeg'),
  (Ext: '.html'; MimeType: 'text/html'), (Ext: '.htm'; MimeType: 'text/html'), (Ext: '.css'; MimeType: 'text/css'),
  (Ext: '.js'; MimeType: 'text/javascript'), (Ext: '.txt'; MimeType: 'text/plain'), (Ext: '.xls';
  MimeType: 'application/excel'), (Ext: '.rtf'; MimeType: 'text/richtext'), (Ext: '.wq1';
  MimeType: 'application/x-lotus'), (Ext: '.wk1'; MimeType: 'application/x-lotus'), (Ext: '.raf';
  MimeType: 'application/raf'), (Ext: '.png'; MimeType: 'image/x-png'), (Ext: '.c'; MimeType: 'text/plain'),
  (Ext: '.c++'; MimeType: 'text/plain'), (Ext: '.pl'; MimeType: 'text/plain'), (Ext: '.cc'; MimeType: 'text/plain'),
  (Ext: '.h'; MimeType: 'text/plain'), (Ext: '.talk'; MimeType: 'text/x-speech'), (Ext: '.xbm';
  MimeType: 'image/x-xbitmap'), (Ext: '.xpm'; MimeType: 'image/x-xpixmap'), (Ext: '.ief'; MimeType: 'image/ief'),
  (Ext: '.jpe'; MimeType: 'image/jpeg'), (Ext: '.tiff'; MimeType: 'image/tiff'), (Ext: '.tif'; MimeType: 'image/tiff'),
  (Ext: '.rgb'; MimeType: 'image/rgb'), (Ext: '.g3f'; MimeType: 'image/g3fax'), (Ext: '.xwd';
  MimeType: 'image/x-xwindowdump'), (Ext: '.pict'; MimeType: 'image/x-pict'), (Ext: '.ppm';
  MimeType: 'image/x-portable-pixmap'), (Ext: '.pgm'; MimeType: 'image/x-portable-graymap'), (Ext: '.pbm';
  MimeType: 'image/x-portable-bitmap'), (Ext: '.pnm'; MimeType: 'image/x-portable-anymap'), (Ext: '.bmp';
  MimeType: 'image/x-ms-bmp'), (Ext: '.ras'; MimeType: 'image/x-cmu-raster'), (Ext: '.pcd';
  MimeType: 'image/x-photo-cd'), (Ext: '.cgm'; MimeType: 'image/cgm'), (Ext: '.mil'; MimeType: 'image/x-cals'),
  (Ext: '.cal'; MimeType: 'image/x-cals'), (Ext: '.fif'; MimeType: 'image/fif'), (Ext: '.dsf';
  MimeType: 'image/x-mgx-dsf'), (Ext: '.cmx'; MimeType: 'image/x-cmx'), (Ext: '.wi'; MimeType: 'image/wavelet'),
  (Ext: '.dwg'; MimeType: 'image/vnd.dwg'), (Ext: '.dxf'; MimeType: 'image/vnd.dxf'), (Ext: '.svf';
  MimeType: 'image/vnd.svf'), (Ext: '.au'; MimeType: 'audio/basic'), (Ext: '.snd'; MimeType: 'audio/basic'),
  (Ext: '.aif'; MimeType: 'audio/x-aiff'), (Ext: '.aiff'; MimeType: 'audio/x-aiff'), (Ext: '.aifc';
  MimeType: 'audio/x-aiff'), (Ext: '.wav'; MimeType: 'audio/x-wav'), (Ext: '.mpa'; MimeType: 'audio/x-mpeg'),
  (Ext: '.abs'; MimeType: 'audio/x-mpeg'), (Ext: '.mpega'; MimeType: 'audio/x-mpeg'), (Ext: '.mp2a';
  MimeType: 'audio/x-mpeg-2'), (Ext: '.mpa2'; MimeType: 'audio/x-mpeg-2'), (Ext: '.es'; MimeType: 'audio/echospeech'),
  (Ext: '.vox'; MimeType: 'audio/voxware'), (Ext: '.lcc'; MimeType: 'application/fastman'), (Ext: '.ra';
  MimeType: 'application/x-pn-realaudio'), (Ext: '.ram'; MimeType: 'application/x-pn-realaudio'), (Ext: '.mmid';
  MimeType: 'x-music/x-midi'), (Ext: '.skp'; MimeType: 'application/vnd.koan'), (Ext: '.talk';
  MimeType: 'text/x-speech'), (Ext: '.mpeg'; MimeType: 'video/mpeg'), (Ext: '.mpg'; MimeType: 'video/mpeg'),
  (Ext: '.mpe'; MimeType: 'video/mpeg'), (Ext: '.mpv2'; MimeType: 'video/mpeg-2'), (Ext: '.mp2v';
  MimeType: 'video/mpeg-2'), (Ext: '.qt'; MimeType: 'video/quicktime'), (Ext: '.mov'; MimeType: 'video/quicktime'),
  (Ext: '.avi'; MimeType: 'video/x-msvideo'), (Ext: '.movie'; MimeType: 'video/x-sgi-movie'), (Ext: '.vdo';
  MimeType: 'video/vdo'), (Ext: '.viv'; MimeType: 'video/vnd.vivo'), (Ext: '.pac';
  MimeType: 'application/x-ns-proxy-autoconfig'), (Ext: '.ai'; MimeType: 'application/postscript'), (Ext: '.eps';
  MimeType: 'application/postscript'), (Ext: '.ps'; MimeType: 'application/postscript'), (Ext: '.rtf';
  MimeType: 'application/rtf'), (Ext: '.pdf'; MimeType: 'application/pdf'), (Ext: '.mif';
  MimeType: 'application/vnd.mif'), (Ext: '.t'; MimeType: 'application/x-troff'), (Ext: '.tr';
  MimeType: 'application/x-troff'), (Ext: '.roff'; MimeType: 'application/x-troff'), (Ext: '.man';
  MimeType: 'application/x-troff-man'), (Ext: '.me'; MimeType: 'application/x-troff-me'), (Ext: '.ms';
  MimeType: 'application/x-troff-ms'), (Ext: '.latex'; MimeType: 'application/x-latex'), (Ext: '.tex';
  MimeType: 'application/x-tex'), (Ext: '.texinfo'; MimeType: 'application/x-texinfo'), (Ext: '.texi';
  MimeType: 'application/x-texinfo'), (Ext: '.dvi'; MimeType: 'application/x-dvi'), (Ext: '.doc';
  MimeType: 'application/msword'), (Ext: '.oda'; MimeType: 'application/oda'), (Ext: '.evy';
  MimeType: 'application/envoy'), (Ext: '.gtar'; MimeType: 'application/x-gtar'), (Ext: '.tar';
  MimeType: 'application/x-tar'), (Ext: '.ustar'; MimeType: 'application/x-ustar'), (Ext: '.bcpio';
  MimeType: 'application/x-bcpio'), (Ext: '.cpio'; MimeType: 'application/x-cpio'), (Ext: '.shar';
  MimeType: 'application/x-shar'), (Ext: '.zip'; MimeType: 'application/zip'), (Ext: '.hqx';
  MimeType: 'application/mac-binhex40'), (Ext: '.sit'; MimeType: 'application/x-stuffit'), (Ext: '.sea';
  MimeType: 'application/x-stuffit'), (Ext: '.fif'; MimeType: 'application/fractals'), (Ext: '.bin';
  MimeType: 'application/octet-stream'), (Ext: '.uu'; MimeType: 'application/octet-stream'), (Ext: '.exe';
  MimeType: 'application/octet-stream'), (Ext: '.src'; MimeType: 'application/x-wais-source'), (Ext: '.wsrc';
  MimeType: 'application/x-wais-source'), (Ext: '.hdf'; MimeType: 'application/hdf'), (Ext: '.ls';
  MimeType: 'text/javascript'), (Ext: '.mocha'; MimeType: 'text/javascript'), (Ext: '.vbs'; MimeType: 'text/vbscript'),
  (Ext: '.sh'; MimeType: 'application/x-sh'), (Ext: '.csh'; MimeType: 'application/x-csh'), (Ext: '.pl';
  MimeType: 'application/x-perl'), (Ext: '.tcl'; MimeType: 'application/x-tcl'), (Ext: '.spl';
  MimeType: 'application/futuresplash'), (Ext: '.mbd'; MimeType: 'application/mbedlet'), (Ext: '.swf';
  MimeType: 'application/x-director'), (Ext: '.pps'; MimeType: 'application/mspowerpoint'), (Ext: '.asp';
  MimeType: 'application/x-asap'), (Ext: '.asn'; MimeType: 'application/astound'), (Ext: '.axs';
  MimeType: 'application/x-olescript'), (Ext: '.ods'; MimeType: 'application/x-oleobject'), (Ext: '.opp';
  MimeType: 'x-form/x-openscape'), (Ext: '.wba'; MimeType: 'application/x-webbasic'), (Ext: '.frm';
  MimeType: 'application/x-alpha-form'), (Ext: '.wfx'; MimeType: 'x-script/x-wfxclient'), (Ext: '.pcn';
  MimeType: 'application/x-pcn'), (Ext: '.ppt'; MimeType: 'application/vnd.ms-powerpoint'), (Ext: '.svd';
  MimeType: 'application/vnd.svd'), (Ext: '.ins'; MimeType: 'application/x-net-install'), (Ext: '.ccv';
  MimeType: 'application/ccv'), (Ext: '.vts'; MimeType: 'workbook/formulaone'), (Ext: '.wrl';
  MimeType: 'x-world/x-vrml'), (Ext: '.vrml'; MimeType: 'x-world/x-vrml'), (Ext: '.vrw'; MimeType: 'x-world/x-vream'),
  (Ext: '.p3d'; MimeType: 'application/x-p3d'), (Ext: '.svr'; MimeType: 'x-world/x-svr'), (Ext: '.wvr';
  MimeType: 'x-world/x-wvr'), (Ext: '.3dmf'; MimeType: 'x-world/x-3dmf'), (Ext: '.ma';
  MimeType: 'application/mathematica'), (Ext: '.msh'; MimeType: 'x-model/x-mesh'), (Ext: '.v5d';
  MimeType: 'application/vis5d'), (Ext: '.igs'; MimeType: 'application/iges'), (Ext: '.dwf'; MimeType: 'drawing/x-dwf'),
  (Ext: '.showcase'; MimeType: 'application/x-showcase'), (Ext: '.slides'; MimeType: 'application/x-showcase'),
  (Ext: '.sc'; MimeType: 'application/x-showcase'), (Ext: '.sho'; MimeType: 'application/x-showcase'), (Ext: '.show';
  MimeType: 'application/x-showcase'), (Ext: '.ins'; MimeType: 'application/x-insight'), (Ext: '.insight';
  MimeType: 'application/x-insight'), (Ext: '.ano'; MimeType: 'application/x-annotator'), (Ext: '.dir';
  MimeType: 'application/x-dirview'), (Ext: '.lic'; MimeType: 'application/x-enterlicense'), (Ext: '.faxmgr';
  MimeType: 'application/x-fax-manager'), (Ext: '.faxmgrjob'; MimeType: 'application/x-fax-manager-job'),
  (Ext: '.icnbk'; MimeType: 'application/x-iconbook'), (Ext: '.wb'; MimeType: 'application/x-inpview'), (Ext: '.inst';
  MimeType: 'application/x-install'), (Ext: '.mail'; MimeType: 'application/x-mailfolder'), (Ext: '.pp';
  MimeType: 'application/x-ppages'), (Ext: '.ppages'; MimeType: 'application/x-ppages'), (Ext: '.sgi-lpr';
  MimeType: 'application/x-sgi-lpr'), (Ext: '.tardist'; MimeType: 'application/x-tardist'), (Ext: '.ztardist';
  MimeType: 'application/x-ztardist'), (Ext: '.wkz'; MimeType: 'application/x-wingz'), (Ext: '.xml';
  MimeType: 'application/xml'), (Ext: '.iv'; MimeType: 'graphics/x-inventor'));

function GetFileMimeType(const AFileName, ADefaultType: string): string;
var
  LExtension: string;
  I: Integer;
begin
  Result := ADefaultType;
  LExtension := LowerCase(ExtractFileExt(AFileName));
  for I := Low(MIME_TYPES) to High(MIME_TYPES) do
    if MIME_TYPES[I].Ext = LExtension then
      Exit(MIME_TYPES[I].MimeType);
end;

end.
