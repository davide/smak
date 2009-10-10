%% @author Hunter Morris <hunter.morris@smarkets.com>
%% @copyright 2009 Smarkets Limited.
%%
%% @doc Smak media types.  Resources can use the pre-defined media types here for consistency.
%% @end
%%
%% Licensed under the MIT license:
%% http://www.opensource.org/licenses/mit-license.php

-module(smak_media_types).
-author('Hunter Morris <hunter.morris@smarkets.com>').

-include_lib("eunit/include/eunit.hrl").

-export([main_type/1, sub_type/1, equals/2, equals/3, includes/2,
         is_concrete/1, to_string/1, is_compatible/2,
         most_specific/1]).
-export([mime_type/1, mime_type/2]).
-export([description/1, description/2]).
-export([parameters/1, parameters/2]).
-export([media_type/2, media_type/3]).
-export([all/0, application_all/0, application_all_xml/0,
         application_atom_service_xml/0, application_atom_xml/0,
         application_cab/0, application_compress/0,
         application_excel/0, application_flash/0,
         application_gnu_tar/0, application_gnu_zip/0,
         application_http_cookies/0, application_java/0,
         application_java_archive/0, application_java_object/0,
         application_javascript/0, application_jnlp/0,
         application_json/0, application_latex/0,
         application_mac_binhex40/0, application_mathml_xml/0,
         application_octet_stream/0, application_pdf/0,
         application_postscript/0, application_powerpoint/0,
         application_project/0, application_rdf_xml/0,
         application_relaxng_compact/0, application_relaxng_xml/0,
         application_rss_xml/0, application_rtf/0,
         application_stuffit/0, application_tar/0, application_tex/0,
         application_troff_man/0, application_voicexml/0,
         application_w3c_schema_xml/0, application_w3c_xslt/0,
         application_wadl_xml/0, application_word/0,
         application_www_form/0, application_xhtml_xml/0,
         application_xml/0, application_xml_dtd/0, application_xul/0,
         application_zip/0, audio_all/0, audio_basic/0, audio_midi/0,
         audio_mpeg/0, audio_real/0, audio_wav/0, image_all/0,
         image_bmp/0, image_gif/0, image_icon/0, image_jpeg/0,
         image_png/0, image_svg/0, image_tiff/0, message_all/0,
         message_http/0, message_imdn_xml/0, model_all/0,
         model_vrml/0, multipart_all/0, multipart_byteranges/0,
         multipart_form_data/0, text_all/0, text_calendar/0,
         text_css/0, text_csv/0, text_html/0,
         text_j2me_app_descriptor/0, text_javascript/0, text_plain/0,
         text_rdf_n3/0, text_uri_list/0, text_vcard/0, text_xml/0,
         video_all/0, video_avi/0, video_mp4/0, video_mpeg/0,
         video_quicktime/0, video_wmv/0]).

-include("smak.hrl").

-define(_ALL, #media_type{mime="*/*", description="All media"}).

%%----------------------------------------------------------------------
%% Pre-defined media types
%%----------------------------------------------------------------------

-spec all() -> #media_type{}.
all() ->
    ?_ALL.

%% @spec application_all() -> media_type()
%% @doc Media type for: All application documents
-spec application_all() -> #media_type{}.
application_all() ->
    media_type("application/*", "All application documents").

%% @spec application_all_xml() -> media_type()
%% @doc Media type for: All application/*+xml documents
-spec application_all_xml() -> #media_type{}.
application_all_xml() ->
    media_type("application/*+xml", "All application/*+xml documents").

%% @spec application_atom_service_xml() -> media_type()
%% @doc Media type for: Atom service documents
-spec application_atom_service_xml() -> #media_type{}.
application_atom_service_xml() ->
    media_type("application/atomsvc+xml", "Atom service documents").

%% @spec application_atom_xml() -> media_type()
%% @doc Media type for: Atom syndication documents
-spec application_atom_xml() -> #media_type{}.
application_atom_xml() ->
    media_type("application/atom+xml", "Atom syndication documents").

%% @spec application_cab() -> media_type()
%% @doc Media type for: Microsoft Cabinet archive
-spec application_cab() -> #media_type{}.
application_cab() ->
    media_type("application/vnd.ms-cab-compressed", "Microsoft Cabinet archive").

%% @spec application_compress() -> media_type()
%% @doc Media type for: Compressed filed
-spec application_compress() -> #media_type{}.
application_compress() ->
    media_type("application/x-compress", "Compressed filed").

%% @spec application_excel() -> media_type()
%% @doc Media type for: Microsoft Excel document
-spec application_excel() -> #media_type{}.
application_excel() ->
    media_type("application/vnd.ms-excel", "Microsoft Excel document").

%% @spec application_flash() -> media_type()
%% @doc Media type for: Shockwave Flash object
-spec application_flash() -> #media_type{}.
application_flash() ->
    media_type("application/x-shockwave-flash", "Shockwave Flash object").

%% @spec application_gnu_tar() -> media_type()
%% @doc Media type for: GNU Tar archive
-spec application_gnu_tar() -> #media_type{}.
application_gnu_tar() ->
    media_type("application/x-gtar", "GNU Tar archive").

%% @spec application_gnu_zip() -> media_type()
%% @doc Media type for: GNU Zip archive
-spec application_gnu_zip() -> #media_type{}.
application_gnu_zip() ->
    media_type("application/x-gzip", "GNU Zip archive").

%% @spec application_http_cookies() -> media_type()
%% @doc Media type for: HTTP cookies
-spec application_http_cookies() -> #media_type{}.
application_http_cookies() ->
    media_type("application/x-http-cookies", "HTTP cookies").

%% @spec application_java() -> media_type()
%% @doc Media type for: Java class
-spec application_java() -> #media_type{}.
application_java() ->
    media_type("application/java", "Java class").

%% @spec application_java_archive() -> media_type()
%% @doc Media type for: Java archive
-spec application_java_archive() -> #media_type{}.
application_java_archive() ->
    media_type("application/java-archive", "Java archive").

%% @spec application_java_object() -> media_type()
%% @doc Media type for: Java serialized object
-spec application_java_object() -> #media_type{}.
application_java_object() ->
    media_type("application/x-java-serialized-object", "Java serialized object").

%% @spec application_javascript() -> media_type()
%% @doc Media type for: Javascript document
-spec application_javascript() -> #media_type{}.
application_javascript() ->
    media_type("application/x-javascript", "Javascript document").

%% @spec application_jnlp() -> media_type()
%% @doc Media type for: JNLP
-spec application_jnlp() -> #media_type{}.
application_jnlp() ->
    media_type("application/x-java-jnlp-file", "JNLP").

%% @spec application_json() -> media_type()
%% @doc Media type for: JavaScript Object Notation document
-spec application_json() -> #media_type{}.
application_json() ->
    media_type("application/json", "JavaScript Object Notation document").

%% @spec application_latex() -> media_type()
%% @doc Media type for: LaTeX
-spec application_latex() -> #media_type{}.
application_latex() ->
    media_type("application/x-latex", "LaTeX").

%% @spec application_mac_binhex40() -> media_type()
%% @doc Media type for: Mac binhex40
-spec application_mac_binhex40() -> #media_type{}.
application_mac_binhex40() ->
    media_type("application/mac-binhex40", "Mac binhex40").

%% @spec application_mathml_xml() -> media_type()
%% @doc Media type for: Mathml XML document
-spec application_mathml_xml() -> #media_type{}.
application_mathml_xml() ->
    media_type("application/mathml+xml", "Mathml XML document").

%% @spec application_octet_stream() -> media_type()
%% @doc Media type for: Raw octet stream
-spec application_octet_stream() -> #media_type{}.
application_octet_stream() ->
    media_type("application/octet-stream", "Raw octet stream").

%% @spec application_pdf() -> media_type()
%% @doc Media type for: Adobe PDF document
-spec application_pdf() -> #media_type{}.
application_pdf() ->
    media_type("application/pdf", "Adobe PDF document").

%% @spec application_postscript() -> media_type()
%% @doc Media type for: Postscript document
-spec application_postscript() -> #media_type{}.
application_postscript() ->
    media_type("application/postscript", "Postscript document").

%% @spec application_powerpoint() -> media_type()
%% @doc Media type for: Microsoft Powerpoint document
-spec application_powerpoint() -> #media_type{}.
application_powerpoint() ->
    media_type("application/vnd.ms-powerpoint", "Microsoft Powerpoint document").

%% @spec application_project() -> media_type()
%% @doc Media type for: Microsoft Project document
-spec application_project() -> #media_type{}.
application_project() ->
    media_type("application/vnd.ms-project", "Microsoft Project document").

%% @spec application_rdf_xml() -> media_type()
%% @doc Media type for: XML serialized Resource Description Framework document
-spec application_rdf_xml() -> #media_type{}.
application_rdf_xml() ->
    media_type("application/rdf+xml", "XML serialized Resource Description Framework document").

%% @spec application_relaxng_compact() -> media_type()
%% @doc Media type for: Relax NG Schema document, Compact syntax
-spec application_relaxng_compact() -> #media_type{}.
application_relaxng_compact() ->
    media_type("application/relax-ng-compact-syntax", "Relax NG Schema document, Compact syntax").

%% @spec application_relaxng_xml() -> media_type()
%% @doc Media type for: Relax NG Schema document, XML syntax
-spec application_relaxng_xml() -> #media_type{}.
application_relaxng_xml() ->
    media_type("application/x-relax-ng+xml", "Relax NG Schema document, XML syntax").

%% @spec application_rss_xml() -> media_type()
%% @doc Media type for: Really Simple Syndication document
-spec application_rss_xml() -> #media_type{}.
application_rss_xml() ->
    media_type("application/rss+xml", "Really Simple Syndication document").

%% @spec application_rtf() -> media_type()
%% @doc Media type for: Rich Text Format document
-spec application_rtf() -> #media_type{}.
application_rtf() ->
    media_type("application/rtf", "Rich Text Format document").

%% @spec application_stuffit() -> media_type()
%% @doc Media type for: Stuffit archive
-spec application_stuffit() -> #media_type{}.
application_stuffit() ->
    media_type("application/x-stuffit", "Stuffit archive").

%% @spec application_tar() -> media_type()
%% @doc Media type for: Tar archive
-spec application_tar() -> #media_type{}.
application_tar() ->
    media_type("application/x-tar", "Tar archive").

%% @spec application_tex() -> media_type()
%% @doc Media type for: Tex file
-spec application_tex() -> #media_type{}.
application_tex() ->
    media_type("application/x-tex", "Tex file").

%% @spec application_troff_man() -> media_type()
%% @doc Media type for: LaTeX
-spec application_troff_man() -> #media_type{}.
application_troff_man() ->
    media_type("application/x-troff-man", "LaTeX").

%% @spec application_voicexml() -> media_type()
%% @doc Media type for: VoiceXML
-spec application_voicexml() -> #media_type{}.
application_voicexml() ->
    media_type("application/voicexml+xml", "VoiceXML").

%% @spec application_w3c_schema_xml() -> media_type()
%% @doc Media type for: W3C XML Schema document
-spec application_w3c_schema_xml() -> #media_type{}.
application_w3c_schema_xml() ->
    media_type("application/x-xsd+xml", "W3C XML Schema document").

%% @spec application_w3c_xslt() -> media_type()
%% @doc Media type for: W3C XSLT Stylesheet
-spec application_w3c_xslt() -> #media_type{}.
application_w3c_xslt() ->
    media_type("application/xsd+xml", "W3C XSLT Stylesheet").

%% @spec application_wadl_xml() -> media_type()
%% @doc Media type for: Web Application Description Language document
-spec application_wadl_xml() -> #media_type{}.
application_wadl_xml() ->
    media_type("application/vnd.sun.wadl+xml", "Web Application Description Language document").

%% @spec application_word() -> media_type()
%% @doc Media type for: Microsoft Word document
-spec application_word() -> #media_type{}.
application_word() ->
    media_type("application/msword", "Microsoft Word document").

%% @spec application_www_form() -> media_type()
%% @doc Media type for: Web form (URL encoded)
-spec application_www_form() -> #media_type{}.
application_www_form() ->
    media_type("application/x-www-form-urlencoded", "Web form (URL encoded)").

%% @spec application_xhtml_xml() -> media_type()
%% @doc Media type for: XHTML document
-spec application_xhtml_xml() -> #media_type{}.
application_xhtml_xml() ->
    media_type("application/xhtml+xml", "XHTML document").

%% @spec application_xml() -> media_type()
%% @doc Media type for: XML document
-spec application_xml() -> #media_type{}.
application_xml() ->
    media_type("application/xml", "XML document").

%% @spec application_xml_dtd() -> media_type()
%% @doc Media type for: XML DTD
-spec application_xml_dtd() -> #media_type{}.
application_xml_dtd() ->
    media_type("application/xml-dtd", "XML DTD").

%% @spec application_xul() -> media_type()
%% @doc Media type for: XUL document
-spec application_xul() -> #media_type{}.
application_xul() ->
    media_type("application/vnd.mozilla.xul+xml", "XUL document").

%% @spec application_zip() -> media_type()
%% @doc Media type for: Zip archive
-spec application_zip() -> #media_type{}.
application_zip() ->
    media_type("application/zip", "Zip archive").

%% @spec audio_all() -> media_type()
%% @doc Media type for: All audios
-spec audio_all() -> #media_type{}.
audio_all() ->
    media_type("audio/*", "All audios").

%% @spec audio_basic() -> media_type()
%% @doc Media type for: AU audio
-spec audio_basic() -> #media_type{}.
audio_basic() ->
    media_type("audio/basic", "AU audio").

%% @spec audio_midi() -> media_type()
%% @doc Media type for: MIDI audio
-spec audio_midi() -> #media_type{}.
audio_midi() ->
    media_type("audio/midi", "MIDI audio").

%% @spec audio_mpeg() -> media_type()
%% @doc Media type for: MPEG audio (MP3)
-spec audio_mpeg() -> #media_type{}.
audio_mpeg() ->
    media_type("audio/mpeg", "MPEG audio (MP3)").

%% @spec audio_real() -> media_type()
%% @doc Media type for: Real audio
-spec audio_real() -> #media_type{}.
audio_real() ->
    media_type("audio/x-pn-realaudio", "Real audio").

%% @spec audio_wav() -> media_type()
%% @doc Media type for: Waveform audio
-spec audio_wav() -> #media_type{}.
audio_wav() ->
    media_type("audio/x-wav", "Waveform audio").

%% @spec image_all() -> media_type()
%% @doc Media type for: All images
-spec image_all() -> #media_type{}.
image_all() ->
    media_type("image/*", "All images").

%% @spec image_bmp() -> media_type()
%% @doc Media type for: Windows bitmap
-spec image_bmp() -> #media_type{}.
image_bmp() ->
    media_type("image/bmp", "Windows bitmap").

%% @spec image_gif() -> media_type()
%% @doc Media type for: GIF image
-spec image_gif() -> #media_type{}.
image_gif() ->
    media_type("image/gif", "GIF image").

%% @spec image_icon() -> media_type()
%% @doc Media type for: Windows icon (Favicon)
-spec image_icon() -> #media_type{}.
image_icon() ->
    media_type("image/x-icon", "Windows icon (Favicon)").

%% @spec image_jpeg() -> media_type()
%% @doc Media type for: JPEG image
-spec image_jpeg() -> #media_type{}.
image_jpeg() ->
    media_type("image/jpeg", "JPEG image").

%% @spec image_png() -> media_type()
%% @doc Media type for: PNG image
-spec image_png() -> #media_type{}.
image_png() ->
    media_type("image/png", "PNG image").

%% @spec image_svg() -> media_type()
%% @doc Media type for: Scalable Vector Graphics
-spec image_svg() -> #media_type{}.
image_svg() ->
    media_type("image/svg+xml", "Scalable Vector Graphics").

%% @spec image_tiff() -> media_type()
%% @doc Media type for: TIFF image
-spec image_tiff() -> #media_type{}.
image_tiff() ->
    media_type("image/tiff", "TIFF image").

%% @spec message_all() -> media_type()
%% @doc Media type for: All messages
-spec message_all() -> #media_type{}.
message_all() ->
    media_type("message/*", "All messages").

%% @spec message_http() -> media_type()
%% @doc Media type for: HTTP message
-spec message_http() -> #media_type{}.
message_http() ->
    media_type("message/http", "HTTP message").

%% @spec message_imdn_xml() -> media_type()
%% @doc Media type for: Instant Message Disposition Notification
message_imdn_xml() ->
    media_type("message/imdn+xml", "Instant Message Disposition Notification").

%% @spec model_all() -> media_type()
%% @doc Media type for: All models
-spec model_all() -> #media_type{}.
model_all() ->
    media_type("model/*", "All models").

%% @spec model_vrml() -> media_type()
%% @doc Media type for: VRML
-spec model_vrml() -> #media_type{}.
model_vrml() ->
    media_type("model/vrml", "VRML").

%% @spec multipart_all() -> media_type()
%% @doc Media type for: All multipart data
-spec multipart_all() -> #media_type{}.
multipart_all() ->
    media_type("multipart/*", "All multipart data").

%% @spec multipart_form_data() -> media_type()
%% @doc Media type for: Multipart form data
-spec multipart_form_data() -> #media_type{}.
multipart_form_data() ->
    media_type("multipart/form-data", "Multipart form data").

%% @spec multipart_byteranges() -> media_type()
%% @doc Media type for: Content of multiple byte ranges
-spec multipart_byteranges() -> #media_type{}.
multipart_byteranges() ->
    media_type("multipart/byteranges", "Content of multiple byte ranges").    

%% @spec text_all() -> media_type()
%% @doc Media type for: All texts
-spec text_all() -> #media_type{}.
text_all() ->
    media_type("text/*", "All texts").

%% @spec text_calendar() -> media_type()
%% @doc Media type for: iCalendar event
-spec text_calendar() -> #media_type{}.
text_calendar() ->
    media_type("text/calendar", "iCalendar event").

%% @spec text_css() -> media_type()
%% @doc Media type for: CSS stylesheet
-spec text_css() -> #media_type{}.
text_css() ->
    media_type("text/css", "CSS stylesheet").

%% @spec text_csv() -> media_type()
%% @doc Media type for: Comma-Separated Values
-spec text_csv() -> #media_type{}.
text_csv() ->
    media_type("text/csv", "Comma-Separated Values").

%% @spec text_html() -> media_type()
%% @doc Media type for: HTML document
-spec text_html() -> #media_type{}.
text_html() ->
    media_type("text/html", "HTML document").

%% @spec text_j2me_app_descriptor() -> media_type()
%% @doc Media type for: J2ME Application Descriptor
-spec text_j2me_app_descriptor() -> #media_type{}.
text_j2me_app_descriptor() ->
    media_type("text/vnd.sun.j2me.app-descriptor", "J2ME Application Descriptor").

%% @spec text_javascript() -> media_type()
%% @doc Media type for: Javascript document
-spec text_javascript() -> #media_type{}.
text_javascript() ->
    media_type("text/javascript", "Javascript document").

%% @spec text_plain() -> media_type()
%% @doc Media type for: Plain text
-spec text_plain() -> #media_type{}.
text_plain() ->
    media_type("text/plain", "Plain text").

%% @spec text_rdf_n3() -> media_type()
%% @doc Media type for: N3 serialized Resource Description Framework document
-spec text_rdf_n3() -> #media_type{}.
text_rdf_n3() ->
    media_type("text/rdf+n3", "N3 serialized Resource Description Framework document").

%% @spec text_uri_list() -> media_type()
%% @doc Media type for: List of URIs
-spec text_uri_list() -> #media_type{}.
text_uri_list() ->
    media_type("text/uri-list", "List of URIs").

%% @spec text_vcard() -> media_type()
%% @doc Media type for: vCard
-spec text_vcard() -> #media_type{}.
text_vcard() ->
    media_type("text/x-vcard", "vCard").

%% @spec text_xml() -> media_type()
%% @doc Media type for: XML text
-spec text_xml() -> #media_type{}.
text_xml() ->
    media_type("text/xml", "XML text").

%% @spec video_all() -> media_type()
%% @doc Media type for: All videos
-spec video_all() -> #media_type{}.
video_all() ->
    media_type("video/*", "All videos").

%% @spec video_avi() -> media_type()
%% @doc Media type for: AVI video
-spec video_avi() -> #media_type{}.
video_avi() ->
    media_type("video/x-msvideo", "AVI video").

%% @spec video_mp4() -> media_type()
%% @doc Media type for: MPEG-4 video
-spec video_mp4() -> #media_type{}.
video_mp4() ->
    media_type("video/mp4", "MPEG-4 video").

%% @spec video_mpeg() -> media_type()
%% @doc Media type for: MPEG video
-spec video_mpeg() -> #media_type{}.
video_mpeg() ->
    media_type("video/mpeg", "MPEG video").

%% @spec video_quicktime() -> media_type()
%% @doc Media type for: Quicktime video
-spec video_quicktime() -> #media_type{}.
video_quicktime() ->
    media_type("video/quicktime", "Quicktime video").

%% @spec video_wmv() -> media_type()
%% @doc Media type for: Windows movie
-spec video_wmv() -> #media_type{}.
video_wmv() ->
    media_type("video/x-ms-wmv", "Windows movie").

%%----------------------------------------------------------------------
%% Utilities
%%----------------------------------------------------------------------

%% @spec main_type(MediaType::media_type()) -> string()
%% @doc Returns the main media type
-spec main_type(#media_type{}) -> string().
main_type(#media_type{mime=Mime}) when is_list(Mime), length(Mime) > 0 ->
    Index = case string:chr(Mime, $/) of
                0 ->
                    string:chr(Mime, $;);
                N when N > 0 ->
                    N
            end,
    if Index =:= 0 ->
            Mime;
       true ->
            string:substr(Mime, 1, Index - 1)
    end.

%% @spec sub_type(MediaType::media_type()) -> string()
%% @doc Returns the media sub-type
-spec sub_type(#media_type{}) -> string().
sub_type(#media_type{mime=Mime}) when is_list(Mime), length(Mime) > 0 ->
    case string:chr(Mime, $/) of
        0 ->
            "*";
        Slash ->
            case string:chr(Mime, $;) of
                0 ->
                    string:substr(Mime, Slash + 1);
                Sep ->
                    string:substr(Mime, Slash + 1, Sep - Slash - 1)
            end
    end.

%% @spec parameters(MediaType::media_type()) -> [{string(), string()}]
%% @doc Returns the parameters for MediaType
-spec parameters(#media_type{}) -> [{string(), string()}].
parameters(#media_type{parameters=P}) ->
    P.

%% @spec parameters(Params::[{string(), string()}],
%%                  MediaType::media_type()) -> media_type()
%% @doc Sets the parameters for MediaType to Params
-spec parameters([{string(), string()}], #media_type{}) -> #media_type{}.
parameters(V, M) ->
    M#media_type{parameters=V}.

%% @spec mime_type(MediaType::media_type()) -> string()
%% @doc Returns the MIME type for MediaType
-spec mime_type(#media_type{}) -> string().
mime_type(#media_type{mime=V}) ->
    V.

%% @spec mime_type(MimeType::string(),
%%                  MediaType::media_type()) -> media_type()
%% @doc Sets the MIME type for MediaType to MimeType
-spec mime_type(string(), #media_type{}) -> #media_type{}.
mime_type(V, M) ->
    M#media_type{mime=V}.

%% @spec description(MediaType::media_type()) -> string()
%% @doc Returns the description for MediaType
-spec description(#media_type{}) -> string().
description(#media_type{description=V}) ->
    V.

%% @spec description(Desc::string(),
%%                   MediaType::media_type()) -> media_type()
%% @doc Sets the description for MediaType to Desc
-spec description(string(), #media_type{}) -> #media_type{}.
description(V, M) ->
    M#media_type{description=V}.

%% @spec includes(Left::media_type(), Right::media_type()) -> bool()
%% @doc Returns whether or not a given media type is included in the
%% current one. It is true if both types are equal or if the right
%% media type is within the left one.
-spec includes(#media_type{}, #media_type{}) -> bool().
includes(?_ALL, _) ->
    true;
includes(L, R) ->
    case equals(L, R) of
        true ->
            true;
        false ->
            includes(main_type(L), sub_type(L), main_type(R), sub_type(R))
    end.

-spec includes(string(), string(), string(), string()) -> bool().
includes(LMain, LSub, LMain, LSub) ->
    true;
includes(LMain, "*", LMain, _) ->
    true;
includes(LMain, "*+" ++ LSub, LMain, RSub) ->
    LSubR = lists:reverse(LSub),
    RSubR = lists:reverse(RSub),
    includes1(LSubR, RSubR);
includes(_, _, _, _) ->
    false.

-spec includes1(string(), string()) -> bool().
includes1([], _) ->
    true;
includes1([H|L], [H|R]) ->
    includes1(L, R);
includes1(_, _) ->
    false.

%% @spec equals(Left::media_type(), Right::media_type()) -> bool()
%% @equiv equals(L, R, false)
-spec equals(#media_type{}, #media_type{}) -> bool().
equals(L, R) ->
    equals(L, R, false).

%% @spec equals(Left::media_type(),
%%              Right::media_type(),
%%              IgnoreParams::bool()) -> bool()
%% @doc Returns whether or not two media types are equal.  If
%% IgnoreParams is false, checks parameters for equality.
-spec equals(#media_type{}, #media_type{}, bool()) -> bool().
equals(L, L, _) when is_record(L, media_type) ->
    true;
equals(#media_type{mime=LMime, description=LDesc},
       #media_type{mime=RMime, description=RDesc}, _)
  when LMime =/= RMime; LDesc =/= RDesc ->
    false;
equals(_, _, true) ->
    true;
equals(#media_type{parameters=L}, #media_type{parameters=R}, false) ->
    lists:sort(L) =:= lists:sort(R).

%% @spec is_compatible(Left::media_type(), Right::media_type()) -> bool()
%% @doc Checks if two media types are compatible
-spec is_compatible(#media_type{}, #media_type{}) -> bool().
is_compatible(L, R) ->
    case includes(L, R) of
        true ->
            true;
        false ->
            includes(R, L)
    end.

%% @spec is_concrete(Type::media_type()) -> bool()
%% @doc Checks if a media type is concrete (i.e. does not include a
%% wildcard)
-spec is_concrete(#media_type{}) -> bool().
is_concrete(#media_type{mime=M}) ->
    string:chr(M, $*) =:= 0.

%% @spec to_string(Type::media_type()) -> string()
%% @doc Returns the string representation of a media type
-spec to_string(#media_type{}) -> string().
to_string(#media_type{mime=M, parameters=[]}) ->
    M;
to_string(#media_type{mime=M, parameters=P}) ->
    ParamString = [[K, $=, V] || {K, V} <- P],
    lists:flatten([M, "; ", ParamString]). % TODO: performance?

%% @spec media_type(MimeType::string(),
%%                  Description::string()) -> media_type()
%% @equiv media_type(MimeType, Description, [])
-spec media_type(string(), string()) -> #media_type{}.
media_type(Mime, Description) ->
    media_type(Mime, Description, []).

%% @spec media_type(MimeType::string(),
%%                  Description::string(),
%%                  Parameters::[{string(), string()}]) -> media_type()
%% @doc Returns the media type record with the given MIME type,
%% description, and parameters.
-spec media_type(string(), string(),
                 [{string(), string()}]) -> #media_type{}.
media_type(Mime, Description, Parameters) ->
    #media_type{mime=Mime,
                description=Description,
                parameters=Parameters}.

%% @spec most_specific(MediaTypes::[media_type()]) -> media_type()
%% @doc Returns the first, most specific media type for a given list
%% MediaTypes.
-spec most_specific([#media_type{}]) -> #media_type{}.
most_specific(L) when is_list(L), length(L) > 1 ->
    lists:foldr(fun most_specific/2, undefined, L);
most_specific([M]) when is_record(M, media_type) ->
    M.

-spec most_specific(#media_type{},
                    'undefined' | #media_type{}) -> #media_type{}.
most_specific(M, undefined) ->
    M;
most_specific(M, Most) when is_record(Most, media_type) ->
    case main_type(M) of
        "*" ->
            Most;
        _ ->
            case main_type(Most) of
                "*" ->
                    M;
                _ ->
                    case string:chr(sub_type(Most), $*) of
                        0 ->
                            Most;
                        _ ->
                            M
                    end
            end
    end.

%%----------------------------------------------------------------------
%% Unit tests (TODO: move to smak_media_types_tests)
%%----------------------------------------------------------------------

concrete_test_() ->
    [?_assertNot(is_concrete(all())),
     ?_assertNot(is_concrete(application_all())),
     ?_assertNot(is_concrete(application_all_xml())),
     ?_assert(is_concrete(application_atom_service_xml())),
     ?_assert(is_concrete(application_atom_xml())),
     ?_assert(is_concrete(application_cab())),
     ?_assert(is_concrete(application_compress())),
     ?_assert(is_concrete(application_excel())),
     ?_assert(is_concrete(application_flash())),
     ?_assert(is_concrete(application_gnu_tar())),
     ?_assert(is_concrete(application_gnu_zip())),
     ?_assert(is_concrete(application_http_cookies())),
     ?_assert(is_concrete(application_java())),
     ?_assert(is_concrete(application_java_archive())),
     ?_assert(is_concrete(application_java_object())),
     ?_assert(is_concrete(application_javascript())),
     ?_assert(is_concrete(application_jnlp())),
     ?_assert(is_concrete(application_json())),
     ?_assert(is_concrete(application_latex())),
     ?_assert(is_concrete(application_mac_binhex40())),
     ?_assert(is_concrete(application_mathml_xml())),
     ?_assert(is_concrete(application_octet_stream())),
     ?_assert(is_concrete(application_pdf())),
     ?_assert(is_concrete(application_postscript())),
     ?_assert(is_concrete(application_powerpoint())),
     ?_assert(is_concrete(application_project())),
     ?_assert(is_concrete(application_rdf_xml())),
     ?_assert(is_concrete(application_relaxng_compact())),
     ?_assert(is_concrete(application_relaxng_xml())),
     ?_assert(is_concrete(application_rss_xml())),
     ?_assert(is_concrete(application_rtf())),
     ?_assert(is_concrete(application_stuffit())),
     ?_assert(is_concrete(application_tar())),
     ?_assert(is_concrete(application_tex())),
     ?_assert(is_concrete(application_troff_man())),
     ?_assert(is_concrete(application_voicexml())),
     ?_assert(is_concrete(application_w3c_schema_xml())),
     ?_assert(is_concrete(application_w3c_xslt())),
     ?_assert(is_concrete(application_wadl_xml())),
     ?_assert(is_concrete(application_word())),
     ?_assert(is_concrete(application_www_form())),
     ?_assert(is_concrete(application_xhtml_xml())),
     ?_assert(is_concrete(application_xml())),
     ?_assert(is_concrete(application_xml_dtd())),
     ?_assert(is_concrete(application_xul())),
     ?_assert(is_concrete(application_zip())),
     ?_assertNot(is_concrete(audio_all())),
     ?_assert(is_concrete(audio_basic())),
     ?_assert(is_concrete(audio_midi())),
     ?_assert(is_concrete(audio_mpeg())),
     ?_assert(is_concrete(audio_real())),
     ?_assert(is_concrete(audio_wav())),
     ?_assertNot(is_concrete(image_all())),
     ?_assert(is_concrete(image_bmp())),
     ?_assert(is_concrete(image_gif())),
     ?_assert(is_concrete(image_icon())),
     ?_assert(is_concrete(image_jpeg())),
     ?_assert(is_concrete(image_png())),
     ?_assert(is_concrete(image_svg())),
     ?_assert(is_concrete(image_tiff())),
     ?_assertNot(is_concrete(message_all())),
     ?_assert(is_concrete(message_http())),
     ?_assert(is_concrete(message_imdn_xml())),
     ?_assertNot(is_concrete(model_all())),
     ?_assert(is_concrete(model_vrml())),
     ?_assertNot(is_concrete(multipart_all())),
     ?_assert(is_concrete(multipart_byteranges())),
     ?_assert(is_concrete(multipart_form_data())),
     ?_assertNot(is_concrete(text_all())),
     ?_assert(is_concrete(text_calendar())),
     ?_assert(is_concrete(text_css())),
     ?_assert(is_concrete(text_csv())),
     ?_assert(is_concrete(text_html())),
     ?_assert(is_concrete(text_j2me_app_descriptor())),
     ?_assert(is_concrete(text_javascript())),
     ?_assert(is_concrete(text_plain())),
     ?_assert(is_concrete(text_rdf_n3())),
     ?_assert(is_concrete(text_uri_list())),
     ?_assert(is_concrete(text_vcard())),
     ?_assert(is_concrete(text_xml())),
     ?_assertNot(is_concrete(video_all())),
     ?_assert(is_concrete(video_avi())),
     ?_assert(is_concrete(video_mp4())),
     ?_assert(is_concrete(video_mpeg())),
     ?_assert(is_concrete(video_quicktime())),
     ?_assert(is_concrete(video_wmv()))].

main_type_test_() ->
    [?_assertEqual("*", main_type(all())),
     ?_assertEqual("application", main_type(application_all())),
     ?_assertEqual("application", main_type(application_all_xml())),
     ?_assertEqual("application", main_type(application_atom_service_xml())),
     ?_assertEqual("application", main_type(application_atom_xml())),
     ?_assertEqual("application", main_type(application_cab())),
     ?_assertEqual("application", main_type(application_compress())),
     ?_assertEqual("application", main_type(application_excel())),
     ?_assertEqual("application", main_type(application_flash())),
     ?_assertEqual("application", main_type(application_gnu_tar())),
     ?_assertEqual("application", main_type(application_gnu_zip())),
     ?_assertEqual("application", main_type(application_http_cookies())),
     ?_assertEqual("application", main_type(application_java())),
     ?_assertEqual("application", main_type(application_java_archive())),
     ?_assertEqual("application", main_type(application_java_object())),
     ?_assertEqual("application", main_type(application_javascript())),
     ?_assertEqual("application", main_type(application_jnlp())),
     ?_assertEqual("application", main_type(application_json())),
     ?_assertEqual("application", main_type(application_latex())),
     ?_assertEqual("application", main_type(application_mac_binhex40())),
     ?_assertEqual("application", main_type(application_mathml_xml())),
     ?_assertEqual("application", main_type(application_octet_stream())),
     ?_assertEqual("application", main_type(application_pdf())),
     ?_assertEqual("application", main_type(application_postscript())),
     ?_assertEqual("application", main_type(application_powerpoint())),
     ?_assertEqual("application", main_type(application_project())),
     ?_assertEqual("application", main_type(application_rdf_xml())),
     ?_assertEqual("application", main_type(application_relaxng_compact())),
     ?_assertEqual("application", main_type(application_relaxng_xml())),
     ?_assertEqual("application", main_type(application_rss_xml())),
     ?_assertEqual("application", main_type(application_rtf())),
     ?_assertEqual("application", main_type(application_stuffit())),
     ?_assertEqual("application", main_type(application_tar())),
     ?_assertEqual("application", main_type(application_tex())),
     ?_assertEqual("application", main_type(application_troff_man())),
     ?_assertEqual("application", main_type(application_voicexml())),
     ?_assertEqual("application", main_type(application_w3c_schema_xml())),
     ?_assertEqual("application", main_type(application_w3c_xslt())),
     ?_assertEqual("application", main_type(application_wadl_xml())),
     ?_assertEqual("application", main_type(application_word())),
     ?_assertEqual("application", main_type(application_www_form())),
     ?_assertEqual("application", main_type(application_xhtml_xml())),
     ?_assertEqual("application", main_type(application_xml())),
     ?_assertEqual("application", main_type(application_xml_dtd())),
     ?_assertEqual("application", main_type(application_xul())),
     ?_assertEqual("application", main_type(application_zip())),
     ?_assertEqual("audio", main_type(audio_all())),
     ?_assertEqual("audio", main_type(audio_basic())),
     ?_assertEqual("audio", main_type(audio_midi())),
     ?_assertEqual("audio", main_type(audio_mpeg())),
     ?_assertEqual("audio", main_type(audio_real())),
     ?_assertEqual("audio", main_type(audio_wav())),
     ?_assertEqual("image", main_type(image_all())),
     ?_assertEqual("image", main_type(image_bmp())),
     ?_assertEqual("image", main_type(image_gif())),
     ?_assertEqual("image", main_type(image_icon())),
     ?_assertEqual("image", main_type(image_jpeg())),
     ?_assertEqual("image", main_type(image_png())),
     ?_assertEqual("image", main_type(image_svg())),
     ?_assertEqual("image", main_type(image_tiff())),
     ?_assertEqual("message", main_type(message_all())),
     ?_assertEqual("message", main_type(message_http())),
     ?_assertEqual("message", main_type(message_imdn_xml())),
     ?_assertEqual("model", main_type(model_all())),
     ?_assertEqual("model", main_type(model_vrml())),
     ?_assertEqual("multipart", main_type(multipart_all())),
     ?_assertEqual("multipart", main_type(multipart_byteranges())),
     ?_assertEqual("multipart", main_type(multipart_form_data())),
     ?_assertEqual("text", main_type(text_all())),
     ?_assertEqual("text", main_type(text_calendar())),
     ?_assertEqual("text", main_type(text_css())),
     ?_assertEqual("text", main_type(text_csv())),
     ?_assertEqual("text", main_type(text_html())),
     ?_assertEqual("text", main_type(text_j2me_app_descriptor())),
     ?_assertEqual("text", main_type(text_javascript())),
     ?_assertEqual("text", main_type(text_plain())),
     ?_assertEqual("text", main_type(text_rdf_n3())),
     ?_assertEqual("text", main_type(text_uri_list())),
     ?_assertEqual("text", main_type(text_vcard())),
     ?_assertEqual("text", main_type(text_xml())),
     ?_assertEqual("video", main_type(video_all())),
     ?_assertEqual("video", main_type(video_avi())),
     ?_assertEqual("video", main_type(video_mp4())),
     ?_assertEqual("video", main_type(video_mpeg())),
     ?_assertEqual("video", main_type(video_quicktime())),
     ?_assertEqual("video", main_type(video_wmv()))].

sub_type_test_() ->
    [?_assertEqual("*", sub_type(all())),
     ?_assertEqual("*", sub_type(application_all())),
     ?_assertEqual("*+xml", sub_type(application_all_xml())),
     ?_assertEqual("atomsvc+xml", sub_type(application_atom_service_xml())),
     ?_assertEqual("atom+xml", sub_type(application_atom_xml())),
     ?_assertEqual("vnd.ms-cab-compressed", sub_type(application_cab())),
     ?_assertEqual("x-compress", sub_type(application_compress())),
     ?_assertEqual("vnd.ms-excel", sub_type(application_excel())),
     ?_assertEqual("x-shockwave-flash", sub_type(application_flash())),
     ?_assertEqual("x-gtar", sub_type(application_gnu_tar())),
     ?_assertEqual("x-gzip", sub_type(application_gnu_zip())),
     ?_assertEqual("x-http-cookies", sub_type(application_http_cookies())),
     ?_assertEqual("java", sub_type(application_java())),
     ?_assertEqual("java-archive", sub_type(application_java_archive())),
     ?_assertEqual("x-java-serialized-object", sub_type(application_java_object())),
     ?_assertEqual("x-javascript", sub_type(application_javascript())),
     ?_assertEqual("x-java-jnlp-file", sub_type(application_jnlp())),
     ?_assertEqual("json", sub_type(application_json())),
     ?_assertEqual("x-latex", sub_type(application_latex())),
     ?_assertEqual("mac-binhex40", sub_type(application_mac_binhex40())),
     ?_assertEqual("mathml+xml", sub_type(application_mathml_xml())),
     ?_assertEqual("octet-stream", sub_type(application_octet_stream())),
     ?_assertEqual("pdf", sub_type(application_pdf())),
     ?_assertEqual("postscript", sub_type(application_postscript())),
     ?_assertEqual("vnd.ms-powerpoint", sub_type(application_powerpoint())),
     ?_assertEqual("vnd.ms-project", sub_type(application_project())),
     ?_assertEqual("rdf+xml", sub_type(application_rdf_xml())),
     ?_assertEqual("relax-ng-compact-syntax", sub_type(application_relaxng_compact())),
     ?_assertEqual("x-relax-ng+xml", sub_type(application_relaxng_xml())),
     ?_assertEqual("rss+xml", sub_type(application_rss_xml())),
     ?_assertEqual("rtf", sub_type(application_rtf())),
     ?_assertEqual("x-stuffit", sub_type(application_stuffit())),
     ?_assertEqual("x-tar", sub_type(application_tar())),
     ?_assertEqual("x-tex", sub_type(application_tex())),
     ?_assertEqual("x-troff-man", sub_type(application_troff_man())),
     ?_assertEqual("voicexml+xml", sub_type(application_voicexml())),
     ?_assertEqual("x-xsd+xml", sub_type(application_w3c_schema_xml())),
     ?_assertEqual("xsd+xml", sub_type(application_w3c_xslt())),
     ?_assertEqual("vnd.sun.wadl+xml", sub_type(application_wadl_xml())),
     ?_assertEqual("msword", sub_type(application_word())),
     ?_assertEqual("x-www-form-urlencoded", sub_type(application_www_form())),
     ?_assertEqual("xhtml+xml", sub_type(application_xhtml_xml())),
     ?_assertEqual("xml", sub_type(application_xml())),
     ?_assertEqual("xml-dtd", sub_type(application_xml_dtd())),
     ?_assertEqual("vnd.mozilla.xul+xml", sub_type(application_xul())),
     ?_assertEqual("zip", sub_type(application_zip())),
     ?_assertEqual("*", sub_type(audio_all())),
     ?_assertEqual("basic", sub_type(audio_basic())),
     ?_assertEqual("midi", sub_type(audio_midi())),
     ?_assertEqual("mpeg", sub_type(audio_mpeg())),
     ?_assertEqual("x-pn-realaudio", sub_type(audio_real())),
     ?_assertEqual("x-wav", sub_type(audio_wav())),
     ?_assertEqual("*", sub_type(image_all())),
     ?_assertEqual("bmp", sub_type(image_bmp())),
     ?_assertEqual("gif", sub_type(image_gif())),
     ?_assertEqual("x-icon", sub_type(image_icon())),
     ?_assertEqual("jpeg", sub_type(image_jpeg())),
     ?_assertEqual("png", sub_type(image_png())),
     ?_assertEqual("svg+xml", sub_type(image_svg())),
     ?_assertEqual("tiff", sub_type(image_tiff())),
     ?_assertEqual("*", sub_type(message_all())),
     ?_assertEqual("http", sub_type(message_http())),
     ?_assertEqual("imdn+xml", sub_type(message_imdn_xml())),
     ?_assertEqual("*", sub_type(model_all())),
     ?_assertEqual("vrml", sub_type(model_vrml())),
     ?_assertEqual("*", sub_type(multipart_all())),
     ?_assertEqual("form-data", sub_type(multipart_form_data())),
     ?_assertEqual("byteranges", sub_type(multipart_byteranges())),
     ?_assertEqual("*", sub_type(text_all())),
     ?_assertEqual("calendar", sub_type(text_calendar())),
     ?_assertEqual("css", sub_type(text_css())),
     ?_assertEqual("csv", sub_type(text_csv())),
     ?_assertEqual("html", sub_type(text_html())),
     ?_assertEqual("vnd.sun.j2me.app-descriptor", sub_type(text_j2me_app_descriptor())),
     ?_assertEqual("javascript", sub_type(text_javascript())),
     ?_assertEqual("plain", sub_type(text_plain())),
     ?_assertEqual("rdf+n3", sub_type(text_rdf_n3())),
     ?_assertEqual("uri-list", sub_type(text_uri_list())),
     ?_assertEqual("x-vcard", sub_type(text_vcard())),
     ?_assertEqual("xml", sub_type(text_xml())),
     ?_assertEqual("*", sub_type(video_all())),
     ?_assertEqual("x-msvideo", sub_type(video_avi())),
     ?_assertEqual("mp4", sub_type(video_mp4())),
     ?_assertEqual("mpeg", sub_type(video_mpeg())),
     ?_assertEqual("quicktime", sub_type(video_quicktime())),
     ?_assertEqual("x-ms-wmv", sub_type(video_wmv()))].

equals_test_() ->
    [?_assert(equals(all(), all())),
     ?_assert(equals(multipart_form_data(), multipart_form_data())),
     ?_assert(equals(text_javascript(), text_javascript())),
     ?_assertNot(equals(text_javascript(), text_all())),
     ?_assertNot(equals(text_all(), text_javascript())),
     ?_assertNot(equals(image_png(), image_jpeg())),
     ?_assertNot(equals(video_mp4(), video_wmv()))].

includes_test_() ->
    [?_assert(includes(text_all(), text_plain())),
     ?_assertNot(includes(text_plain(), text_all())),
     ?_assert(includes(all(), message_imdn_xml()))].

is_compatible_test_() ->
    [?_assert(is_compatible(text_all(), text_plain())),
     ?_assert(is_compatible(text_plain(), text_all())),
     ?_assertNot(is_compatible(text_plain(), application_all()))].

parts_test_() ->
    TextPlain = text_plain(),
    [?_assertEqual(TextPlain, media_type(mime_type(TextPlain),
                                         description(TextPlain))),
     ?_assertEqual(TextPlain, media_type(mime_type(TextPlain),
                                         description(TextPlain),
                                         parameters(TextPlain)))].

most_specific_test_() ->
    [?_assertEqual(text_plain(), most_specific([text_plain(), text_all(), all()])),
     ?_assertEqual(text_all(), most_specific([all(), text_all()])),
     ?_assertEqual(video_mpeg(), most_specific([video_all(), video_mpeg(), all()])),
     ?_assertEqual(all(), most_specific([all()])),
     ?_assertError(function_clause, most_specific([]))].
