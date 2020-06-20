#nullable enable
using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Net;
using System.Net.Http;
using System.Text;
using System.Threading.Tasks;

using AngleSharp;
using AngleSharp.Dom;
using AngleSharp.Html;
using AngleSharp.Html.Dom;
using AngleSharp.Html.Parser;
using AngleSharp.Io;

using Engineering.DataSource;

using Newtonsoft.Json;

using OilGas.Data;

namespace OilGas.Data.RRC.Texas
{
    public static class CompletionReportType
    {
        public const string W1  = "W1";
        public const string W2  = "W2";
        public const string W4  = "W4";
        public const string W10 = "W10"; //
        public const string W14 = "W14";
        public const string G1  = "G1"; //
        public const string G5  = "G5";
        public const string G10 = "G10";
        public const string L1  = "L1"; //
        public const string P1  = "P1";
        public const string P4  = "P4"; //
        public const string P6  = "P6"; //
        public const string P15 = "P15";
    }

    ////dynamic map service layer
    //operationalLayerUrl: "https://gis.rrc.texas.gov/server/rest/services/rrc_public/RRC_Public_Viewer_Srvs/MapServer",
    ////operationalLayerUrl: "https://gis.rrc.texas.gov/server/rest/services/rrc_public/RRC_Web_Direct_temp/MapServer",
    ////basemap of initial Map
    ////baseMapUrl: "https://services.arcgisonline.com/ArcGIS/rest/services/NatGeo_World_Map/MapServer",
    //baseMapUrl: "https://services.arcgisonline.com/ArcGIS/rest/services/World_Street_Map/MapServer",
    //baseMapUrlForSurvey: "https://services.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer",
    //baseMapUrlForSurveyLabels: "https://services.arcgisonline.com/ArcGIS/rest/services/Reference/World_Boundaries_and_Places/MapServer",
    ////geometry service
    //geometryserviceurl: "https://gis.rrc.texas.gov/server/rest/services/Utilities/Geometry/GeometryServer",
    //// print options
    //printtask: "https://gis.rrc.texas.gov/server/rest/services/Utilities/PrintingTools/GPServer/Export%20Web%20Map%20Task",
    //placefinder: {
    //    //"url": "https://tasks.arcgis.com/ArcGIS/rest/services/WorldLocator/GeocodeServer",
    //    "url": "https://geocode.arcgis.com/arcgis/rest/services/World/GeocodeServer/findAddressCandidates?",
    //    "singlelinefieldname": "SingleLine"
    //},

    //customlogo: {
    //    image: 'GISViewer/images/rrc-logo-lg.gif',
    //    link: 'https://www.rrc.texas.gov/'
    //},

    //// completion info and lease query URLs
    //leaseIdSearchUrl: "GISViewer/proxy/proxy.ashx?https://gisutil106p.rrc.texas.gov/GISWebServices/api/OGWellExtract?leaseno=",
    //wellboreUrlTemplate: "GISViewer/proxy/proxy.ashx?https://gisutil106p.rrc.texas.gov/GISWebServices/api/Wellbore?apiNumber=",
    //completionUrlTemplate: "GISViewer/proxy/proxy.ashx?https://gisutil106p.rrc.texas.gov/GISWebServices/api/Completion?apiNumber=",
    //pdqUrlTemplate: "GISViewer/proxy/proxy.ashx?https://gisutil106p.rrc.texas.gov/GISWebServices/api/PdqProdCycle",
    //pdqPath: "http://webapps2.rrc.texas.gov/EWA/specificLeaseQueryAction.do?tab=init&viewType=prodAndTotalDisp&methodToCall=fromGisViewer&pdqSearchArgs.paramValue=|2=", 
    //specialtyUrlTemplate: "GISViewer/proxy/proxy.ashx?https://gisutil106p.rrc.texas.gov/GISWebServices/api/WellboreApilut?leaseNo=",
    //geomSrvUrl: "GISViewer/proxy/proxy.ashx?https://gis.rrc.texas.gov/server/rest/services/Utilities/Geometry/GeometryServer/project?inSR=",
    //detailUrlWellsBase: "http://webapps2.rrc.texas.gov/EWA/leaseDetailAction.do?searchType=apiNo&selTab=2048&apiNo=",
    //detailUrlWells1: "&distCode=",
    //detailUrlWells2: "&leaseNo=",
    //detailUrlWells3: "&methodToCall=displayLeaseDetail",
    ////neubusUrlBase: "https://rrcsearch.neubus.com/esd-rrc/api.php?",        //current link in production; Neubus ESD3.6.0.7
    ////neubusUrlBase: "https://rrcsearch3.neubus.com/esd3.7Pub/api.php?",    //this link is temporarily used by RRC for testing
    //neubusUrlBase: "https://rrcsearch3.neubus.com/esd3-rrc/api.php?",       //the link to be in production after 6/29/2015; Neubus ESD3.7
    //urlFragmentFunctionLogs: "function=GetWellLogs",
    //urlFragmentFunctionLogsforDisposalPermit: "function=GetWellLogs",
    //urlFragmentFunctionImage: "function=GetImage",
    //urlFragmentApiNo: "&api_no=",
    //urlFragmentApiNoforDisposalPermit: "&api_no=",
    //urlFragmentDistrict: "&district=",
    //urlFragmentLeaseId: "&lease_id=",
    //orphanProcedurePdf: "https://www.rrc.texas.gov/media/8874/orphanwelltakeoverprocedure.pdf",
    //drillingPermitUrlBase: "http://webapps.rrc.texas.gov/DP/publicQuerySearchAction.do?countyCode=",  //prod
    ////drillingPermitUrlBase: "http://uat.rrc.texas.gov/DP/publicQuerySearchAction.do?countyCode=",    //uat
    //drillingPermitUrl1: "&apiSeqNo=",
    //wasteDisposalDistictUrl: "https://rrc.texas.gov/oil-gas/applications-and-permits/environmental-permit-types-information/commercial-surface-waste-facilities/commercial-recyclingdisposal-permits-list/district-",
    //rrcDistrictContactUrl: "https://www.rrc.state.tx.us/about-us/organization-activities/rrc-locations/#OilGasLocations",

    public class QueryBuilder
    {
        public const string RrcStateTxBaseUrl = "webapps2.rrc.state.tx.us";

        public const string RrcStateTxPort = "80";

        public const string RrcStateTxSection = "EWA";

        public const string RrcStateTx = "http://" + RrcStateTxBaseUrl + "/" + RrcStateTxSection + "/";

        public const string QueryContentType = "application/x-www-form-urlencoded;charset=utf-8";

        public const string WellboreQuery        = "wellboreQueryAction.do";
        public const string ProductionQuery      = "productionQueryAction.do";
        public const string SpecificLeaseQuery   = "specificLeaseQueryAction.do";
        public const string OrganizationQuery    = "organizationQueryAction.do";
        public const string DrillingPermitsQuery = "drillingPermitsQueryAction.do";
        public const string CompletionsQuery     = "publicSearchAction.do"; //http://webapps.rrc.texas.gov/CMPL/publicSearchAction.do

        //https://gis.rrc.texas.gov/server/rest/services/rrc_public/RRC_Public_Viewer_Srvs/MapServer/0/query?where=API%20in%20%28%2725531594%27%29&outSR=4269&outFields=*&f=pjson

        //http://webapps.rrc.texas.gov/CMPL/viewPdfReportFormAction.do?method=cmplW2FormPdf&packetSummaryId=55368

        //https://rrcsearch3.neubus.com/esd3-rrc/index.php?profile=17&search=%7B%22item%22%3A%5B%7B%22key%22%3A%22lease_number%22%2C%22value%22%3A%2210181%22%7D%2C%7B%22key%22%3A%22district%22%2C%22value%22%3A%2202%22%7D%5D%7D

        public static readonly Uri WellboreQueryAction = new Uri(RrcStateTx + WellboreQuery);

        public static readonly Uri ProductionQueryAction = new Uri(RrcStateTx + ProductionQuery);

        public static readonly Uri SpecificLeaseQueryAction = new Uri(RrcStateTx + SpecificLeaseQuery);

        public static readonly Uri OrganizationQueryAction = new Uri(RrcStateTx + OrganizationQuery);

        public static readonly Uri DrillingPermitsQueryAction = new Uri(RrcStateTx + DrillingPermitsQuery);

        // Completion http://webapps.rrc.texas.gov/CMPL/ewaSearchAction.do
        // Tracking No.: 54106
        // http://webapps.rrc.texas.gov/CMPL/viewPdfReportFormAction.do?method=cmplW2FormPdf&packetSummaryId=54106
        // ACID, FRACTURE, CEMENT SQUEEZE, CAST IRON BRIDGE PLUG, RETAINER, ETC.

        private static readonly CookieContainer cookies;

        private static readonly HttpClientHandler httpClientHandler;

        private static readonly HttpClient client;

        private static readonly WebClient webClient;

        static QueryBuilder()
        {
            //AppContext.SetSwitch("System.Net.Http.UseSocketsHttpHandler", true);

            cookies = new CookieContainer();

            httpClientHandler = new HttpClientHandler()
            {
                //    //Proxy           = new WebProxy(ProxyUrl, false),
                //    PreAuthenticate = true,
                //    Credentials     = CredentialCache.DefaultNetworkCredentials,

                //    //UseDefaultCredentials = true,
                //    AllowAutoRedirect = false
                CookieContainer = cookies
            };

            //client = new HttpClient(httpClientHandler,
            //                        true)
            //{
            //    Timeout = new TimeSpan(0,
            //                           0,
            //                           0,
            //                           0,
            //                           1000)
            //};

            client = new HttpClient(httpClientHandler);

            webClient = new WebClient
            {
                //Proxy       = new WebProxy(ProxyUrl, false),
                Credentials = CredentialCache.DefaultNetworkCredentials
            };
        }

        //private static void GET()
        //{
        //    //var responseString = await client.GetStringAsync("http://www.example.com/recepticle.aspx");
        //}

        private static async Task<IHtmlDocument> CreateDocument(string responseString)
        {
            IConfiguration config = Configuration.Default;

            IBrowsingContext context = BrowsingContext.New(config);

            HtmlParserOptions options = new HtmlParserOptions {IsScripting = context != null && context.IsScripting()};

            HtmlParser parser = new HtmlParser(options, context);

            return await parser.ParseDocumentAsync(responseString);
        }

        public static async Task<IHtmlDocument> OpenUri(string uri)
        {
            string responseString = "HTTP request failed.";

            try
            {
                responseString = await client.GetStringAsync(uri);
            }
            catch(HttpRequestException e)
            {
                Console.WriteLine("\nException Caught!");

                Console.WriteLine("Message :{0} ", e.Message);
            }

            return await CreateDocument(responseString);
        }

        public static async Task<IHtmlDocument> PerformQueryAndCreateDocument(Uri                        requestUri,
                                                                              Dictionary<string, string> request_params)
        {
            string responseString = "HTTP request failed.";

            try
            {
                FormUrlEncodedContent content = new FormUrlEncodedContent(request_params);
                content.Headers.ContentType.CharSet = "utf-8";
                //content.Headers.ContentType = new MediaTypeHeaderValue("application/x-www-form-urlencoded;charset=utf-8");

                //var buffer      = System.Text.Encoding.UTF8.GetBytes(myContent);
                //var byteContent = new ByteArrayContent(buffer);

                HttpResponseMessage response = await client.PostAsync(requestUri, content);

                responseString = await response.Content.ReadAsStringAsync();
            }
            catch(HttpRequestException e)
            {
                Console.WriteLine("\nException Caught!");

                Console.WriteLine("Message :{0} ", e.Message);
            }

            return await CreateDocument(responseString);
        }

        private static readonly JsonSerializerSettings settings = new JsonSerializerSettings
        {
            Formatting = Formatting.Indented, MetadataPropertyHandling = MetadataPropertyHandling.Ignore, DateParseHandling = DateParseHandling.None
        };

        private static string FlattenParameters(Dictionary<string, string> parameters)
        {
            StringBuilder sb = new StringBuilder();

            foreach((string key, string value) in parameters)
            {
                if(string.IsNullOrEmpty(value))
                {
                    sb.Append($"{Uri.EscapeDataString(key)}=&");
                }
                else
                {
                    sb.Append($"{Uri.EscapeDataString(key)}={Uri.EscapeDataString(value)}&");
                }
            }

            string payload = sb.ToString();

            if(payload[^1] == '&')
            {
                payload = payload.Substring(0, payload.Length - 1);
            }

            return payload;
        }

        private static Dictionary<string, string> externalPacketFormParameters()
        {
            // cmplPage TabBox2 DataGrid

            // /CMPL/ewaSearchAction.do

            Dictionary<string, string> parameters = new Dictionary<string, string>
            {
                {"searchArgs.fromSubmitDtArgHndlr.inputValue", ""},
                {"searchArgs.toSubmitDtArgHndlr.inputValue", ""},
                {"searchArgs.districtCodeArgHndlr.inputValue", ""},
                {"searchArgs.packetIdArgHndlr.inputValue", ""},
                {"searchArgs.wellTypeArgHndlr.inputValue", ""},
                {"searchArgs.operatorNoArgHndlr.inputValue", ""},
                {"searchArgs.statusCodeArgHndlr.inputValue", ""},
                {"searchArgs.orderByHndlr.inputValue", ""},
                {"searchArgs.packetTypeCodeHndlr.inputValue", ""},
                {"searchArgs.apiNoHndlr.inputValue", "25532662"},
                {"searchArgs.dpNoHndlr.inputValue", ""},
                {"searchArgs.countyCodeHndlr.inputValue", ""},
                {"searchArgs.leaseNoArgHndlr.inputValue", ""},
                {"searchArgs.fieldNoArgHndlr.inputValue", ""},
                {"searchArgs.wellboreProfileArgHndlr.inputValue", ""},
                {"searchArgs.purposeOfFilingArgHndlr.inputValue", ""},
                {"searchArgs.filingMethodArgHndlr.inputValue", ""},
                {"searchArgs.horzWellboreTypePsaHndlr.booleanValue", "false"},
                {"searchArgs.horzWellboreTypeAllocationHndlr.booleanValue", "false"},
                {"searchArgs.horzWellboreTypeStackedLatHndlr.booleanValue", "false"},
                {"searchArgs.excludeStatusAndApprovedDtHndlr.booleanValue", "false"},
                {"searchArgs.stackedLatStatusNoParentHndlr.inputValue", ""},
                {"methodToCall", "searchByApiNo"},
                {"actionManager.recordCountHndlr.inputValue", "3"},
                {"actionManager.currentIndexHndlr.inputValue", "2"},
                {"actionManager.actionRcrd[0].actionDisplayNmHndlr.inputValue", ""},
                {"actionManager.actionRcrd[0].hostHndlr.inputValue", "webapps2.rrc.texas.gov:80"},
                {"actionManager.actionRcrd[0].contextPathHndlr.inputValue", "/EWA"},
                {"actionManager.actionRcrd[0].actionHndlr.inputValue", "/wellboreQueryAction.do"},
                {"actionManager.actionRcrd[0].actionParameterHndlr.inputValue", "methodToCall"},
                {"actionManager.actionRcrd[0].actionMethodHndlr.inputValue", "returnToSearch"},
                {"actionManager.actionRcrd[0].pagerParameterKeyHndlr.inputValue", ""},
                {"actionManager.actionRcrd[0].actionParametersHndlr.inputValue", ""},
                {"actionManager.actionRcrd[0].returnIndexHndlr.inputValue", "0"},
                {"actionManager.actionRcrd[0].argRcrdParameters(searchArgs.paramValue)", "|7=255|8=32662|9=Y"},
                {"actionManager.actionRcrd[1].actionDisplayNmHndlr.inputValue", ""},
                {"actionManager.actionRcrd[1].hostHndlr.inputValue", "webapps2.rrc.texas.gov:80"},
                {"actionManager.actionRcrd[1].contextPathHndlr.inputValue", "/EWA"},
                {"actionManager.actionRcrd[1].actionHndlr.inputValue", "/wellboreQueryAction.do"},
                {"actionManager.actionRcrd[1].actionParameterHndlr.inputValue", "methodToCall"},
                {"actionManager.actionRcrd[1].actionMethodHndlr.inputValue", "search"},
                {"actionManager.actionRcrd[1].pagerParameterKeyHndlr.inputValue", "wellborePager.paramValue"},
                {"actionManager.actionRcrd[1].actionParametersHndlr.inputValue", ""},
                {"actionManager.actionRcrd[1].returnIndexHndlr.inputValue", "0"},
                {"actionManager.actionRcrd[1].argRcrdParameters(searchArgs.paramValue)", "|7=255|8=32662|9=Y"},
                {"actionManager.actionRcrd[1].argRcrdParameters(wellborePager.paramValue)", "|1=1|2=10|3=1|4=0|5=1|6=10"},
                {"actionManager.actionRcrd[2].actionDisplayNmHndlr.inputValue", ""},
                {"actionManager.actionRcrd[2].hostHndlr.inputValue", "webapps.rrc.texas.gov:80"},
                {"actionManager.actionRcrd[2].contextPathHndlr.inputValue", "/CMPL"},
                {"actionManager.actionRcrd[2].actionHndlr.inputValue", "/ewaSearchAction.do"},
                {"actionManager.actionRcrd[2].actionParameterHndlr.inputValue", "methodToCall"},
                {"actionManager.actionRcrd[2].actionMethodHndlr.inputValue", "searchByApiNo"},
                {"actionManager.actionRcrd[2].pagerParameterKeyHndlr.inputValue", ""},
                {"actionManager.actionRcrd[2].actionParametersHndlr.inputValue", "&amp;apiNo=25532662"},
                {"actionManager.actionRcrd[2].returnIndexHndlr.inputValue", "1"}
            };

            return parameters;
        }

        //public static async Task<T> PerformQuery<T>(Uri                        requestUri,
        //                                            Dictionary<string, string> request_params,
        //                                            StructuredDataExtractor    dataExtractor)
        //{
        //    string responseString = "HTTP request failed.";
        //
        //    try
        //    {
        //        FormUrlEncodedContent content = new FormUrlEncodedContent(request_params);
        //        content.Headers.ContentType.CharSet = "utf-8";
        //        //content.Headers.ContentType = new MediaTypeHeaderValue("application/x-www-form-urlencoded;charset=utf-8");
        //
        //        //var buffer      = System.Text.Encoding.UTF8.GetBytes(myContent);
        //        //var byteContent = new ByteArrayContent(buffer);
        //
        //        HttpResponseMessage response = await client.PostAsync(requestUri, content);
        //
        //        responseString = await response.Content.ReadAsStringAsync();
        //    }
        //    catch(HttpRequestException e)
        //    {
        //        Console.WriteLine("\nException Caught!");
        //
        //        Console.WriteLine("Message :{0} ", e.Message);
        //    }
        //
        //    return JsonConvert.DeserializeObject<T>(JsonConvert.SerializeObject(dataExtractor.Extract(responseString), Formatting.Indented), settings);
        //}

        public static async Task<string> PerformCsvQuery(Uri                        requestUri,
                                                         Dictionary<string, string> request_params)
        {
            string responseString = "HTTP request failed.";

            try
            {
                FormUrlEncodedContent content = new FormUrlEncodedContent(request_params);
                content.Headers.ContentType.CharSet = "utf-8";
                //content.Headers.ContentType = new MediaTypeHeaderValue("application/x-www-form-urlencoded;charset=utf-8");

                //var buffer      = System.Text.Encoding.UTF8.GetBytes(myContent);
                //var byteContent = new ByteArrayContent(buffer);

                HttpResponseMessage response = await client.PostAsync(requestUri, content);

                responseString = await response.Content.ReadAsStringAsync();
            }
            catch(HttpRequestException e)
            {
                Console.WriteLine("\nException Caught!");

                Console.WriteLine("Message :{0} ", e.Message);
            }

            int indexOfTotal = responseString.IndexOf("Total", StringComparison.Ordinal);

            if(indexOfTotal == -1)
            {
                return responseString;
            }

            return responseString.Substring(0, indexOfTotal);
        }

        public static async Task<string> PerformCsvQuery(IHtmlDocument              htmlDocument,
                                                         Dictionary<string, string> request_params)
        {
            string responseString = "HTTP request failed.";

            try
            {
                FormUrlEncodedContent content = new FormUrlEncodedContent(request_params);
                content.Headers.ContentType.CharSet = "utf-8";
                //content.Headers.ContentType = new MediaTypeHeaderValue("application/x-www-form-urlencoded;charset=utf-8");

                //var buffer      = System.Text.Encoding.UTF8.GetBytes(myContent);
                //var byteContent = new ByteArrayContent(buffer);

                HttpResponseMessage response = await client.PostAsync(htmlDocument.DocumentUri, content);

                responseString = await response.Content.ReadAsStringAsync();
            }
            catch(HttpRequestException e)
            {
                Console.WriteLine("\nException Caught!");

                Console.WriteLine("Message :{0} ", e.Message);
            }

            int indexOfTotal = responseString.IndexOf("Total", StringComparison.Ordinal);

            if(indexOfTotal == -1)
            {
                return responseString;
            }

            return responseString.Substring(0, indexOfTotal);
        }

        public static async Task<IHtmlDocument> WellboreQueryByApi(ApiNumber api)
        {
            return await WellboreQueryByApi(api, ScheduleType.Both);
        }

        public static async Task<IHtmlDocument> WellboreQueryByApi(ApiNumber    api,
                                                                   ScheduleType scheduleType)
        {
            string pre = api.CountyCode;
            string suf = api.UniqueWellIdentifier;

            Dictionary<string, string> requestParams = new Dictionary<string, string>
            {
                {"searchArgs.apiNoPrefixArg", pre}, {"searchArgs.apiNoSuffixArg", suf}, {"searchArgs.scheduleTypeArg", scheduleType}, {"methodToCall", "search"}
            };

            return await PerformQueryAndCreateDocument(WellboreQueryAction, requestParams);
        }

        public static async Task<IHtmlDocument> WellboreQueryByWellType(WellType     wellType,
                                                                        DistrictCode districtCode)

        {
            return await WellboreQueryByWellType(wellType, districtCode, ScheduleType.Both);
        }

        public static async Task<IHtmlDocument> WellboreQueryByWellType(WellType     wellType,
                                                                        DistrictCode districtCode,
                                                                        ScheduleType scheduleType)
        {
            Dictionary<string, string> requestParams = new Dictionary<string, string>
            {
                {"searchArgs.districtCodeArg", districtCode.ToString()}, {"searchArgs.wellTypeArg", wellType}, {"searchArgs.scheduleTypeArg", scheduleType}, {"methodToCall", "search"}
            };

            return await PerformQueryAndCreateDocument(WellboreQueryAction, requestParams);
        }

        public static async Task<IHtmlDocument> WellboreQueryByLeaseNumber(string leaseNumber)
        {
            return await WellboreQueryByLeaseNumber(leaseNumber, ScheduleType.Both);
        }

        public static async Task<IHtmlDocument> WellboreQueryByLeaseNumber(string leaseNumber,
                                                                           string districtCode)
        {
            return await WellboreQueryByLeaseNumber(leaseNumber, new DistrictCode(districtCode), ScheduleType.Both);
        }

        public static async Task<IHtmlDocument> WellboreQueryByLeaseNumber(string       leaseNumber,
                                                                           DistrictCode districtCode,
                                                                           ScheduleType scheduleType)
        {
            Dictionary<string, string> requestParams = new Dictionary<string, string>
            {
                {"searchArgs.leaseNumberArg", leaseNumber},
                {"searchArgs.districtCodeArg", districtCode.ToString()},
                {"searchArgs.scheduleTypeArg", scheduleType},
                {"methodToCall", "toLeaseQuery"}
            };

            return await PerformQueryAndCreateDocument(WellboreQueryAction, requestParams);
        }

        public static async Task<IHtmlDocument> WellboreQueryByLeaseNumber(string       leaseNumber,
                                                                           ScheduleType scheduleType)
        {
            Dictionary<string, string> requestParams = new Dictionary<string, string>
            {
                {"searchArgs.leaseNumberArg", leaseNumber}, {"searchArgs.scheduleTypeArg", scheduleType}, {"methodToCall", "toLeaseQuery"}
            };

            return await PerformQueryAndCreateDocument(WellboreQueryAction, requestParams);
        }

        public static LeaseQueryData GetApiFromLeaseNumber(string leaseNumber)
        {
            IHtmlDocument wellboreQueryResult = WellboreQueryByLeaseNumber(leaseNumber).Result;

            WellboreQueryData wellboreQueryData = QueryParser.ParseWellboreQuery(wellboreQueryResult).FirstOrDefault();

            return new LeaseQueryData(wellboreQueryData.Columns.ApiNo, wellboreQueryData.Columns.District, leaseNumber);
        }

        public static LeaseQueryData? GetApiFromLeaseNumber(string leaseNumber,
                                                           string districtCode)
        {
            IHtmlDocument wellboreQueryResult = WellboreQueryByLeaseNumber(leaseNumber, districtCode).Result;

            WellboreQueryData wellboreQueryData = QueryParser.ParseWellboreQuery(wellboreQueryResult).FirstOrDefault();

            if(wellboreQueryData is null)
            {
                return null;
            }

            return new LeaseQueryData(wellboreQueryData.Columns.ApiNo, districtCode, leaseNumber);
        }

        public static async Task<IHtmlDocument> OrganizationNameQuery(string operatorNumber)
        {
            Dictionary<string, string> requestParams = new Dictionary<string, string> {{"number", operatorNumber}, {"methodToCall", "searchByNumber"}};

            //TODO
            //IHtmlDocument organizationNameQueryResult = PerformQuery(OrganizationQueryAction,
            //                                                         requestParams).Result;

            return await PerformQueryAndCreateDocument(OrganizationQueryAction, requestParams);
        }

        public static async Task<LeaseDetailQueryData> LeaseDetailQuery(WellboreQueryData wellboreQueryData)
        {
            IHtmlDocument leaseDetailResult = await OpenUri(RrcStateTx + wellboreQueryData.LeaseDetailAction);

            LeaseDetailQueryData leaseDetailQueryData = QueryParser.ParseLeaseDetailQuery(leaseDetailResult);

            return leaseDetailQueryData;
        }

        public static async Task<(string, LeaseReport)> SpecificLeaseProductionQueryByApi(ApiNumber api)
        {
            return await SpecificLeaseProductionQueryByApi(api, ScheduleType.Both);
        }

        public static async Task<(string, LeaseReport)> SpecificLeaseProductionQueryByApi(ApiNumber    api,
                                                                                          ScheduleType scheduleType)
        {
            IHtmlDocument wellboreQueryResult = WellboreQueryByApi(api, scheduleType).Result;

            WellboreQueryData wellboreQueryData = QueryParser.ParseWellboreQuery(wellboreQueryResult).FirstOrDefault(wqd => wqd.Columns.OnSchedule == "Y");

            if(wellboreQueryData is null)
            {
                throw new Exception("wellboreQueryData is null");
            }

            LeaseDetailQueryData leaseDetailQueryData = await LeaseDetailQuery(wellboreQueryData);

            LeaseReport leaseReport = LeaseReport.Create(wellboreQueryData, leaseDetailQueryData);

            return (await SpecificLeaseProductionQuery(leaseReport), leaseReport);
        }

        public static async Task<string> SpecificLeaseProductionQuery(LeaseReport leaseReport)
        {
            return await SpecificLeaseProductionQuery(leaseReport.LeaseNumber, leaseReport.DistrictCode, leaseReport.LeaseType);
        }

        public static async Task<string> SpecificLeaseProductionQuery(string       leaseNumber,
                                                                      DistrictCode districtCode,
                                                                      LeaseType    leaseType)
        {
            Dictionary<string, string> requestParams = new Dictionary<string, string>
            {
                {"actionManager.actionRcrd[0].actionDisplayNmHndlr.inputValue", "Search Criteria"},
                {"actionManager.actionRcrd[0].actionHndlr.inputValue", "/specificLeaseQueryAction.do"},
                {"actionManager.actionRcrd[0].actionMethodHndlr.inputValue", "unspecified"},
                {"actionManager.actionRcrd[0].actionParameterHndlr.inputValue", "methodToCall"},
                {"actionManager.actionRcrd[0].actionParametersHndlr.inputValue", ""},
                {"actionManager.actionRcrd[0].contextPathHndlr.inputValue", "/EWA"},
                {"actionManager.actionRcrd[0].hostHndlr.inputValue", "webapps2.rrc.state.tx.us:80"},
                {"actionManager.actionRcrd[0].pagerParameterKeyHndlr.inputValue", ""},
                {"actionManager.actionRcrd[0].returnIndexHndlr.inputValue", "0"},
                {"actionManager.currentIndexHndlr.inputValue", "0"},
                {"actionManager.recordCountHndlr.inputValue", "1"},
                {"methodToCall", "generateSpecificLeaseCSVReport"},
                {"searchArgs.activeTabsFlagwordHndlr.inputValue", "0"},
                {"searchArgs.leaseNumberArg", leaseNumber},
                {"searchArgs.districtCodeArg", districtCode.ToString()},
                {"searchArgs.oilOrGasArg", leaseType == LeaseType.Oil ? "O" : "G"},
                {"searchArgs.startMonthArg", "01"},
                {"searchArgs.startYearArg", "1993"},
                {"searchArgs.endMonthArg", DateTime.Now.Month.ToString("D2", CultureInfo.InvariantCulture)},
                {"searchArgs.endYearArg", DateTime.Now.Year.ToString("D4", CultureInfo.InvariantCulture)},
                {"searchArgs.orderByHndlr.inputValue", ""},
                {"searchArgs.searchType", "specificLease"},
                {"searchType", "specificLease"},
                {"submit", "Submit"},
                {"viewType", "init"}
            };

            return await PerformCsvQuery(SpecificLeaseQueryAction, requestParams);
        }

        public static async Task<string> DrillingPermitsQueryByDistrictCode(DistrictCode districtCode,
                                                                            DateTime     approvedDateFrom,
                                                                            DateTime     approvedDateTo)
        {
            return await DrillingPermitsQueryByDistrictCounty(districtCode, null, approvedDateFrom, approvedDateTo);
        }

        public static async Task<string> DrillingPermitsQueryByCounty(CountyType county,
                                                                      DateTime   approvedDateFrom,
                                                                      DateTime   approvedDateTo)
        {
            return await DrillingPermitsQueryByDistrictCounty(null, county, approvedDateFrom, approvedDateTo);
        }

        public static async Task<string> DrillingPermitsQueryByDistrictCounty(DistrictCode? districtCode,
                                                                              CountyType?   county,
                                                                              DateTime      approvedDateFrom,
                                                                              DateTime      approvedDateTo)
        {
            string fromMonth = approvedDateFrom.Month.ToString("D2", CultureInfo.InvariantCulture);
            string fromDay   = approvedDateFrom.Day.ToString("D2", CultureInfo.InvariantCulture);
            string fromYear  = approvedDateFrom.Year.ToString("D4", CultureInfo.InvariantCulture);

            string toMonth = approvedDateTo.Month.ToString("D2", CultureInfo.InvariantCulture);
            string toDay   = approvedDateTo.Day.ToString("D2", CultureInfo.InvariantCulture);
            string toYear  = approvedDateTo.Year.ToString("D4", CultureInfo.InvariantCulture);

            string keyName;
            string keyValue;

            if(county != null)
            {
                keyName  = "searchArgs.countyCodeHndlr.selectedCodes";
                keyValue = $"{(int)county}";
            }
            else
            {
                keyName  = "searchArgs.districtCodeHndlr.inputValue";
                keyValue = $"{(int)districtCode}";
            }

            Dictionary<string, string> requestParams = new Dictionary<string, string>
            {
                {"methodToCall", "search"},
                {"searchArgs.permitStatusNoHndlr.inputValue", ""},
                {"searchArgs.apiNoHndlr.inputValue", ""},
                {keyName, keyValue},
                {"searchArgs.npzFlagHndlr.inputValue", ""},
                {"searchArgs.offLeaseSurfLocFlagHndlr.inputValue", ""},
                {"searchArgs.offLeasePntrnPtFlagHndlr.inputValue", ""},
                {"searchArgs.operatorNameWildcardHndlr.inputValue", "beginsWith"},
                {"searchArgs.operatorNameHndlr.inputValue", ""},
                {"searchArgs.operatorNoHndlr.inputValue", ""},
                {"searchArgs.leaseNameWildcardHndlr.inputValue", "beginsWith"},
                {"searchArgs.leaseNameHndlr.inputValue", ""},
                {"searchArgs.leaseNoHndlr.inputValue", ""},
                {"searchArgs.wellNoHndlr.inputValue", ""},
                {"searchArgs.fieldNameWildcardHndlr.inputValue", "beginsWith"},
                {"searchArgs.fieldNameHndlr.inputValue", ""},
                {"searchArgs.fieldNoHndlr.inputValue", ""},
                {"searchArgs.surveyNameWildcardHndlr.inputValue", "beginsWith"},
                {"searchArgs.surveyNameHndlr.inputValue", ""},
                {"searchArgs.wellTypeCodeHndlr.inputValue", ""},
                {"searchArgs.totalDepthHndlr.inputValue", ""},
                {"searchArgs.filingPurposeCodeHndlr.inputValue", ""},
                {"searchArgs.ammendmentFlagHndlr.inputValue", ""},
                {"searchArgs.statusCodeHndlr.inputValue", ""},
                {"searchArgs.wellboreProfileCodeHndlr.inputValue", ""},
                {"searchArgs.wellLocationCodeHndlr.inputValue", ""},
                {"searchArgs.completionStatusCodeHndlr.inputValue", ""},
                {"searchArgs.psaFlagHndlr.inputValue", ""},
                {"searchArgs.allocationFlagHndlr.inputValue", ""},
                {"searchArgs.stackedLateralFlagHndlr.inputValue", ""},
                {"searchArgs.approvedDtFromHndlr.inputValue", $"{fromMonth}/{fromDay}/{fromYear}"},
                {"searchArgs.approvedDtToHndlr.inputValue", $"{toMonth}/{toDay}/{toYear}"},
                {"searchArgs.submittedDtFromHndlr.inputValue", ""},
                {"searchArgs.submittedDtToHndlr.inputValue", ""}
            };

            IHtmlDocument document = await PerformQueryAndCreateDocument(DrillingPermitsQueryAction, requestParams);

            Dictionary<string, string> requestParams2 = new Dictionary<string, string>
            {
                {"searchArgs.orderByColumnName", ""},
                {"searchArgs.orderByHndlr.inputValue", ""},
                {"searchArgs.universalDocNoHndlr.inputValue", ""},
                {"searchArgs.permitStatusNoHndlr.inputValue", ""},
                {"searchArgs.apiNoHndlr.inputValue", ""},
                {"searchArgs.districtCodeHndlr.inputValue", (districtCode != null ? $"{(int)districtCode}" : "")},
                {"searchArgs.countyCodeHndlr.inputValue", (county         != null ? $"{(int)county}" : "")},
                {"searchArgs.operatorNameWildcardHndlr.inputValue", "beginsWith"},
                {"searchArgs.operatorNameHndlr.inputValue", ""},
                {"searchArgs.operatorNoHndlr.inputValue", ""},
                {"searchArgs.leaseNameWildcardHndlr.inputValue", "beginsWith"},
                {"searchArgs.leaseNameHndlr.inputValue", ""},
                {"searchArgs.leaseNoHndlr.inputValue", ""},
                {"searchArgs.wellNoHndlr.inputValue", ""},
                {"searchArgs.fieldNameWildcardHndlr.inputValue", "beginsWith"},
                {"searchArgs.fieldNameHndlr.inputValue", ""},
                {"searchArgs.fieldNoHndlr.inputValue", ""},
                {"searchArgs.surveyNameWildcardHndlr.inputValue", "beginsWith"},
                {"searchArgs.surveyNameHndlr.inputValue", ""},
                {"searchArgs.wellTypeCodeHndlr.inputValue", ""},
                {"searchArgs.totalDepthHndlr.inputValue", ""},
                {"searchArgs.filingPurposeCodeHndlr.inputValue", ""},
                {"searchArgs.statusCodeHndlr.inputValue", ""},
                {"searchArgs.wellboreProfileCodeHndlr.inputValue", ""},
                {"searchArgs.wellLocationCodeHndlr.inputValue", ""},
                {"searchArgs.completionStatusCodeHndlr.inputValue", ""},
                {"searchArgs.approvedDtFromHndlr.inputValue", $"{fromMonth}/{fromDay}/{fromYear}"},
                {"searchArgs.approvedDtToHndlr.inputValue", $"{toMonth}/{toDay}/{toYear}"},
                {"searchArgs.submittedDtFromHndlr.inputValue", ""},
                {"searchArgs.submittedDtToHndlr.inputValue", ""},
                {"searchArgs.npzFlagHndlr.inputValue", ""},
                {"searchArgs.ammendmentFlagHndlr.inputValue", ""},
                {"searchArgs.swrExceptionCodeHndlr.inputValue", ""},
                {"searchArgs.psaFlagHndlr.inputValue", ""},
                {"searchArgs.allocationFlagHndlr.inputValue", ""},
                {"searchArgs.stackedLateralFlagHndlr.inputValue", ""},
                {"searchArgs.offLeasePntrnPtFlagHndlr.inputValue", ""},
                {"searchArgs.offLeaseSurfLocFlagHndlr.inputValue", ""},
                {"methodToCall", "genDrillingPermitsCSVRpt"},
                {"actionManager.recordCountHndlr.inputValue", "2"},
                {"actionManager.currentIndexHndlr.inputValue", "1"},
                {"actionManager.actionRcrd[0].actionDisplayNmHndlr.inputValue", ""},
                {"actionManager.actionRcrd[0].hostHndlr.inputValue", "webapps2.rrc.texas.gov:80"},
                {"actionManager.actionRcrd[0].contextPathHndlr.inputValue", "/EWA"},
                {"actionManager.actionRcrd[0].actionHndlr.inputValue", "/drillingPermitsQueryAction.do"},
                {"actionManager.actionRcrd[0].actionParameterHndlr.inputValue", "methodToCall"},
                {"actionManager.actionRcrd[0].actionMethodHndlr.inputValue", "returnToSearch"},
                {"actionManager.actionRcrd[0].pagerParameterKeyHndlr.inputValue", ""},
                {"actionManager.actionRcrd[0].actionParametersHndlr.inputValue", ""},
                {"actionManager.actionRcrd[0].returnIndexHndlr.inputValue", "0"},
                {
                    "actionManager.actionRcrd[0].argRcrdParameters(searchArgs.paramValue)",
                    $"|{(county != null ? $"1004={(int)county}" : $"1003={(int)districtCode}")}|1005=beginsWith|1008=beginsWith|1012=beginsWith|1015=beginsWith|1024={fromMonth}/{fromDay}/{fromYear}|1025={toMonth}/{toDay}/{toYear}"
                },
                {"actionManager.actionRcrd[1].actionDisplayNmHndlr.inputValue", ""},
                {"actionManager.actionRcrd[1].hostHndlr.inputValue", "webapps2.rrc.texas.gov:80"},
                {"actionManager.actionRcrd[1].contextPathHndlr.inputValue", "/EWA"},
                {"actionManager.actionRcrd[1].actionHndlr.inputValue", "/drillingPermitsQueryAction.do"},
                {"actionManager.actionRcrd[1].actionParameterHndlr.inputValue", "methodToCall"},
                {"actionManager.actionRcrd[1].actionMethodHndlr.inputValue", "search"},
                {"actionManager.actionRcrd[1].pagerParameterKeyHndlr.inputValue", "pager.paramValue"},
                {"actionManager.actionRcrd[1].actionParametersHndlr.inputValue", ""},
                {"actionManager.actionRcrd[1].returnIndexHndlr.inputValue", "0"},
                {"actionManager.actionRcrd[1].argRcrdParameters(pager.paramValue)", "|1=1|2=10|3=6668|4=0|5=667|6=10"},
                {
                    "actionManager.actionRcrd[1].argRcrdParameters(searchArgs.paramValue)",
                    $"|{(county != null ? $"1004={(int)county}" : $"1003={(int)districtCode}")}|1005=beginsWith|1008=beginsWith|1012=beginsWith|1015=beginsWith|1024={fromMonth}/{fromDay}/{fromYear}|1025={toMonth}/{toDay}/{toYear}"
                },
                {"pager.pageSize", "10"},
                {"propertyValue", ""}
            };

            string payload = "?" + FlattenParameters(requestParams2);

            return await client.GetStringAsync(DrillingPermitsQueryAction + payload);
        }

        public static IEnumerable<string> OrganizationNameQueryByNumber(string operatorNumber)
        {
            return QueryParser.ParseOrganizationNameQuery(OrganizationNameQuery(operatorNumber).Result);
        }

        public static string DirectionalSurveyParameters(string trackingNo)
        {
            Dictionary<string, string> parameters = new Dictionary<string, string>
            {
                {"org.apache.struts.taglib.html.TOKEN", "a592c28abe1fee63279fe193e53bd1aa"},
                {"formData.methodHndlr.inputValue", "dsSearch"},
                {"formData.filingOperatorNoHndlr.inputValue", ""},
                {"formData.filingOperatorNameHndlr.inputValue", ""},
                {"formData.pageForwardHndlr.inputValue", "home"},
                {"formData.pageReturnHndlr.inputValue", ""},
                {"formData.headerTabSelectedHndlr.inputValue", "home"},
                {"formData.resetPagerFlagHndlr.inputValue", ""},
                {"formData.actionFlagHndlr.inputValue", "0"},
                {"searchArgs.packetSummaryIdArgHndlr.inputValue", ""},
                {"searchArgs.operatorNoArgHndlr.inputValue", ""},
                {"searchArgs.apiNoArgHndlr.inputValue", ""},
                {"searchArgs.orderByHndlr.inputValue", ""},
                {"searchArgs.dpIdHndlr.inputValue", ""},
                {"queryArgs.drillingPermitNoArgHndlr.inputValue", ""},
                {"queryArgs.apiNoArgHndlr.inputValue", ""},
                {"queryArgs.operatorNoArgHndlr.inputValue", ""},
                {"queryArgs.orderByHndlr.inputValue", ""},
                {"queryArgs.trackingNoArgHndlr.inputValue", $"{trackingNo}"},
                {"queryArgs.statusArgHndlr.inputValue", "3"},
                {"queryArgs.depthArgHndlr.inputValue", ""},
                {"queryArgs.typeArgHndlr.inputValue", ""},
                {"queryArgs.unUsedCertifiedArgHndlr.inputValue", ""},
                {"queryArgs.unUsedUnCertifiedArgHndlr.inputValue", ""},
                {"queryArgs.internalSearchHndlr.inputValue", "N"},
                {"queryArgs.hardCopyFromDtArgHndlr.inputValue", ""},
                {"queryArgs.hardCopyToDtArgHndlr.inputValue", ""},
                {"queryArgs.unCertifiedHardCopyArgHndlr.inputValue", ""},
                {"queryArgs.certifiedFromDtArgHndlr.inputValue", ""},
                {"queryArgs.certifiedToDtArgHndlr.inputValue", ""},
                {"packetInputRecord.idHndlr.inputValue", ""},
                {"packetInputRecord.createdByHndlr.inputValue", ""},
                {"packetInputRecord.createdDateHndlr.inputValue", ""},
                {"packetInputRecord.createdTimestampHndlr.inputValue", ""},
                {"packetInputRecord.modifiedByHndlr.inputValue", ""},
                {"packetInputRecord.modifiedDateHndlr.inputValue", ""},
                {"packetInputRecord.modifiedTimestampHndlr.inputValue", ""},
                {"packetInputRecord.validationHndlr.inputValue", ""},
                {"packetInputRecord.editedRowHndlr.inputValue", ""},
                {"packetInputRecord.editedGridHndlr.inputValue", ""},
                {"packetInputRecord.packetSummaryIdHndlr.inputValue", ""},
                {"packetInputRecord.formTypeCodeHndlr.inputValue", ""},
                {"packetInputRecord.ctxHndlr.inputValue", "1"},
                {"packetInputRecord.activePageHndlr.inputValue", "1"},
                {"packetInputRecord.currentPageHndlr.inputValue", "1"},
                {"packetInputRecord.lockedByHndlr.inputValue", ""},
                {"packetInputRecord.cycleCodeHndlr.inputValue", ""},
                {"packetInputRecord.rootIdHndlr.inputValue", ""},
                {"packetInputRecord.universalDocNoHndlr.inputValue", ""},
                {"packetInputRecord.drillingPermitNoHndlr.inputValue", ""},
                {"packetInputRecord.operatorNoHndlr.inputValue", ""},
                {"packetInputRecord.theAPINoHndlr.inputValue", ""},
                {"packetInputRecord.wellNoHndlr.inputValue", ""},
                {"packetInputRecord.statusCodeHndlr.inputValue", ""},
                {"packetInputRecord.rule37CaseNoHndlr.inputValue", ""},
                {"packetInputRecord.waterInjectionPermitDateHndlr.inputValue", ""},
                {"packetInputRecord.waterInjectionPermitNoHndlr.inputValue", ""},
                {"packetInputRecord.saltWaterDisposalPermitDateHndlr.inputValue", ""},
                {"packetInputRecord.saltWaterDisposalPermitNoHndlr.inputValue", ""},
                {"packetInputRecord.leaseNoHndlr.inputValue", ""},
                {"packetInputRecord.fieldNoHndlr.inputValue", ""},
                {"packetInputRecord.leaseNameHndlr.inputValue", ""},
                {"packetInputRecord.districtCodeHndlr.inputValue", ""},
                {"packetInputRecord.countyFipsCodeHndlr.inputValue", ""},
                {"packetInputRecord.fieldNameHndlr.inputValue", ""},
                {"packetInputRecord.surveyHndlr.inputValue", ""},
                {"packetInputRecord.formerWellNoCodeHndlr.inputValue", ""},
                {"packetInputRecord.directionNearestTownHndlr.inputValue", ""},
                {"packetInputRecord.rule37CaseDateHndlr.inputValue", ""},
                {"packetInputRecord.latitudeHndlr.inputValue", ""},
                {"packetInputRecord.longitudeHndlr.inputValue", ""},
                {"packetInputRecord.electricLogRunHndlr.inputValue", ""},
                {"packetInputRecord.drillingPermitDateHndlr.inputValue", ""},
                {"packetInputRecord.wellboreProfileHndlr.inputValue", ""},
                {"packetInputRecord.totalAcresHndlr.inputValue", ""},
                {"packetInputRecord.numberOfProducingWellsHndlr.inputValue", ""},
                {"packetInputRecord.nearestWellDistanceHndlr.inputValue", ""},
                {"packetInputRecord.dpOperatorNoHndlr.inputValue", ""},
                {"packetInputRecord.dpFieldNameHndlr.inputValue", ""},
                {"packetInputRecord.latLongCodeHndlr.inputValue", ""},
                {"packetInputRecord.latLongOtherHndlr.inputValue", ""},
                {"packetInputRecord.dpTotalDepthHndlr.inputValue", ""},
                {"packetInputRecord.dpFieldNoHndlr.inputValue", ""},
                {"packetInputRecord.fieldFromHndlr.inputValue", ""},
                {"packetInputRecord.dpLeaseNoHndlr.inputValue", ""},
                {"packetInputRecord.internalUserHndlr.inputValue", "N"},
                {"packetInputRecord.pooledUnitHndlr.inputValue", "N"},
                {"packetInputRecord.oilGasTypeCodeHndlr.inputValue", ""},
                {"packetInputRecord.surveyBlockHndlr.inputValue", ""},
                {"packetInputRecord.surveySectionHndlr.inputValue", ""},
                {"packetInputRecord.surveyAbstractHndlr.inputValue", ""},
                {"packetInputRecord.fieldValidatedDateHndlr.inputValue", ""},
                {"packetInputRecord.unitNoHndlr.inputValue", ""},
                {"packetInputRecord.dpWellNoHndlr.inputValue", ""},
                {"packetInputRecord.origDpNoHndlr.inputValue", ""},
                {"packetInputRecord.origDpDateHndlr.inputValue", ""},
                {"packetInputRecord.mfProcessDateHndlr.inputValue", ""},
                {"packetInputRecord.spudDtHndlr.inputValue", ""},
                {"packetInputRecord.distDirectionMilesHndlr.inputValue", ""},
                {"packetInputRecord.distDirectionDirectionHndlr.inputValue", ""},
                {"packetInputRecord.stackedLatStatusNoParentHndlr.inputValue", ""},
                {"packetInputRecord.fieldHorzSeveranceDepthTxtHndlr.inputValue", ""},
                {"packetInputRecord.latMinTxtHndlr.inputValue", ""},
                {"packetInputRecord.latSecTxtHndlr.inputValue", ""},
                {"packetInputRecord.longMinTxtHndlr.inputValue", ""},
                {"packetInputRecord.longSecTxtHndlr.inputValue", ""},
                {"packetInputRecord.statePlaneXTxtHndlr.inputValue", ""},
                {"packetInputRecord.statePlaneYTxtHndlr.inputValue", ""},
                {"packetInputRecord.statePlaneZoneCodeHndlr.inputValue", ""},
                {"packetInputRecord.fieldHorzSeveranceDepthLowHndlr.inputValue", ""},
                {"packetInputRecord.reservoirDataRowHndlr.inputValue", ""},
                {"packetInputRecord.horzWellboreTypesCatLstHndlr.inputValue", ""},
                {"packetInputRecord.sectionLine1DistanceHndlr.inputValue", ""},
                {"packetInputRecord.sectionLine1DirectionHndlr.inputValue", ""},
                {"packetInputRecord.sectionLine2DistanceHndlr.inputValue", ""},
                {"packetInputRecord.sectionLine2DirectionHndlr.inputValue", ""},
                {"expandId", ""},
                {"deleteId", ""},
                {"expandDetail", "N"},
                {"queryArgs.internalSearch", "N"},
                {"isInternalUser", "N"},
                {"certificationLetterId", ""}
            };

            return "http://webapps.rrc.texas.gov/CMPL/directionalSurveyQueryAction.do?" + FlattenParameters(parameters);
        }

        public static string CompletionReportParameters(string    trackingNo,
                                                        ApiNumber api)
        {
            Dictionary<string, string> parameters = new Dictionary<string, string>
            {
                {"org.apache.struts.taglib.html.TOKEN", "a592c28abe1fee63279fe193e53bd1aa"},
                //{
                //    "methodToCall", "displayBySummaryId"
                //},
                {"packetSummaryId", trackingNo},
                {"formData.methodHndlr.inputValue", "displayBySummaryId"},
                {"formData.filingOperatorNoHndlr.inputValue", ""},
                {"formData.filingOperatorNameHndlr.inputValue", ""},
                {"formData.pageForwardHndlr.inputValue", ""},
                {"formData.pageReturnHndlr.inputValue", ""},
                {"formData.headerTabSelectedHndlr.inputValue", ""},
                {"formData.resetPagerFlagHndlr.inputValue", ""},
                {"formData.actionFlagHndlr.inputValue", "0"},
                {"searchArgs.fromSubmitDtArgHndlr.inputValue", ""},
                {"searchArgs.toSubmitDtArgHndlr.inputValue", ""},
                {"searchArgs.districtCodeArgHndlr.inputValue", ""},
                {"searchArgs.packetIdArgHndlr.inputValue", ""},
                {"searchArgs.wellTypeArgHndlr.inputValue", ""},
                {"searchArgs.operatorNoArgHndlr.inputValue", ""},
                {"searchArgs.statusCodeArgHndlr.inputValue", ""},
                {"searchArgs.orderByHndlr.inputValue", ""},
                {"searchArgs.packetTypeCodeHndlr.inputValue", ""},
                {"searchArgs.apiNoHndlr.inputValue", $"{api.CountyCode}{api.UniqueWellIdentifier}"},
                {"searchArgs.dpNoHndlr.inputValue", ""},
                {"searchArgs.countyCodeHndlr.inputValue", ""},
                {"searchArgs.leaseNoArgHndlr.inputValue", ""},
                {"searchArgs.fieldNoArgHndlr.inputValue", ""},
                {"searchArgs.wellboreProfileArgHndlr.inputValue", ""},
                {"searchArgs.purposeOfFilingArgHndlr.inputValue", ""},
                {"searchArgs.filingMethodArgHndlr.inputValue", ""},
                {"searchArgs.horzWellboreTypePsaHndlr.booleanValue", "false"},
                {"searchArgs.horzWellboreTypeAllocationHndlr.booleanValue", "false"},
                {"searchArgs.horzWellboreTypeStackedLatHndlr.booleanValue", "false"},
                {"searchArgs.excludeStatusAndApprovedDtHndlr.booleanValue", "false"},
                {"searchArgs.stackedLatStatusNoParentHndlr.inputValue", ""},
                {"methodToCall", "displayBySummaryId"},
                {"actionManager.recordCountHndlr.inputValue", "4"},
                {"actionManager.currentIndexHndlr.inputValue", "3"},
                {"actionManager.actionRcrd[0].actionDisplayNmHndlr.inputValue", ""},
                {"actionManager.actionRcrd[0].hostHndlr.inputValue", "webapps2.rrc.state.tx.us:80"},
                {"actionManager.actionRcrd[0].contextPathHndlr.inputValue", "/EWA"},
                {"actionManager.actionRcrd[0].actionHndlr.inputValue", "/wellboreQueryAction.do"},
                {"actionManager.actionRcrd[0].actionParameterHndlr.inputValue", "methodToCall"},
                {"actionManager.actionRcrd[0].actionMethodHndlr.inputValue", "returnToSearch"},
                {"actionManager.actionRcrd[0].pagerParameterKeyHndlr.inputValue", ""},
                {"actionManager.actionRcrd[0].actionParametersHndlr.inputValue", ""},
                {"actionManager.actionRcrd[0].returnIndexHndlr.inputValue", "0"},
                {"actionManager.actionRcrd[0].argRcrdParameters(searchArgs.paramValue)", $"|7={api.CountyCode}|8={api.UniqueWellIdentifier}"},
                {"actionManager.actionRcrd[1].actionDisplayNmHndlr.inputValue", ""},
                {"actionManager.actionRcrd[1].hostHndlr.inputValue", "webapps2.rrc.state.tx.us:80"},
                {"actionManager.actionRcrd[1].contextPathHndlr.inputValue", "/EWA"},
                {"actionManager.actionRcrd[1].actionHndlr.inputValue", "/wellboreQueryAction.do"},
                {"actionManager.actionRcrd[1].actionParameterHndlr.inputValue", "methodToCall"},
                {"actionManager.actionRcrd[1].actionMethodHndlr.inputValue", "search"},
                {"actionManager.actionRcrd[1].pagerParameterKeyHndlr.inputValue", "wellborePager.paramValue"},
                {"actionManager.actionRcrd[1].actionParametersHndlr.inputValue", ""},
                {"actionManager.actionRcrd[1].returnIndexHndlr.inputValue", "0"},
                {"actionManager.actionRcrd[1].argRcrdParameters(searchArgs.paramValue)", $"|7={api.CountyCode}|8={api.UniqueWellIdentifier}"},
                {"actionManager.actionRcrd[1].argRcrdParameters(wellborePager.paramValue)", "|1=1|2=10|3=1|4=0|5=1|6=10"},
                {"actionManager.actionRcrd[2].actionDisplayNmHndlr.inputValue", ""},
                {"actionManager.actionRcrd[2].hostHndlr.inputValue", "webapps.rrc.texas.gov:80"},
                {"actionManager.actionRcrd[2].contextPathHndlr.inputValue", "/CMPL"},
                {"actionManager.actionRcrd[2].actionHndlr.inputValue", "/ewaSearchAction.do"},
                {"actionManager.actionRcrd[2].actionParameterHndlr.inputValue", "methodToCall"},
                {"actionManager.actionRcrd[2].actionMethodHndlr.inputValue", "searchByApiNo"},
                {"actionManager.actionRcrd[2].pagerParameterKeyHndlr.inputValue", ""},
                {"actionManager.actionRcrd[2].actionParametersHndlr.inputValue", $"&amp;apiNo={api.CountyCode}{api.UniqueWellIdentifier}"},
                {"actionManager.actionRcrd[2].returnIndexHndlr.inputValue", "1"},
                {"actionManager.actionRcrd[3].actionDisplayNmHndlr.inputValue", ""},
                {"actionManager.actionRcrd[3].hostHndlr.inputValue", "webapps.rrc.texas.gov:80"},
                {"actionManager.actionRcrd[3].contextPathHndlr.inputValue", "/CMPL"},
                {"actionManager.actionRcrd[3].actionHndlr.inputValue", "/ewaSearchAction.do"},
                {"actionManager.actionRcrd[3].actionParameterHndlr.inputValue", "methodToCall"},
                {"actionManager.actionRcrd[3].actionMethodHndlr.inputValue", "displayBySummaryId"},
                {"actionManager.actionRcrd[3].pagerParameterKeyHndlr.inputValue", ""},
                {"actionManager.actionRcrd[3].actionParametersHndlr.inputValue", ""},
                {"actionManager.actionRcrd[3].returnIndexHndlr.inputValue", "2"}
            };

            return "http://webapps.rrc.texas.gov/CMPL/ewaSearchAction.do?" + FlattenParameters(parameters);
        }

        public static async Task<List<DirectionalSurveyReport>> CompletionReportQueryByApi(ApiNumber api)
        {
            return await CompletionReportQueryByApi(api, ScheduleType.Both);
        }

        //string w2  = $"http://webapps.rrc.texas.gov/CMPL/viewPdfReportFormAction.do?method=cmplW2FormPdf&packetSummaryId={completionsData.Columns.TrackingNo}";
        //string g1  = $"http://webapps.rrc.texas.gov/CMPL/viewPdfReportFormAction.do?method=cmplG1FormPdf&packetSummaryId={completionsData.Columns.TrackingNo}";
        //string l1  = $"http://webapps.rrc.texas.gov/CMPL/viewPdfReportFormAction.do?method=cmplL1FormPdf&packetSummaryId={completionsData.Columns.TrackingNo}";
        //string p4  = $"http://webapps.rrc.texas.gov/CMPL/viewPdfReportFormAction.do?method=cmplP4FormPdf&packetSummaryId={completionsData.Columns.TrackingNo}";
        //string p15 = $"http://webapps.rrc.texas.gov/CMPL/viewPdfReportFormAction.do?method=cmplP15FormPdf&packetSummaryId={completionsData.Columns.TrackingNo}";

        public static async Task<List<DirectionalSurveyReport>> CompletionReportQueryByApi(ApiNumber    api,
                                                                                           ScheduleType scheduleType)
        {
            IHtmlDocument wellboreQueryResult = WellboreQueryByApi(api, scheduleType).Result;

            WellboreQueryData wellboreQueryData = QueryParser.ParseWellboreQuery(wellboreQueryResult).FirstOrDefault(wqd => wqd.Columns.OnSchedule == "Y");

            if(wellboreQueryData is null)
            {
                throw new Exception("wellboreQueryData is null");
            }

            IHtmlDocument completionsQueryResult = await OpenUri(wellboreQueryData.Columns.CompletionLink);

            List<CompletionsQueryData> completionsQueryData = QueryParser.ParseCompletionsQuery(completionsQueryResult);

            List<DirectionalSurveyReport> directionalSurveyReport = new List<DirectionalSurveyReport>();

            IHtmlDocument                    directionalSurveyDocument;
            List<DirectionalSurveyQueryData> directionalSurveyPages;

            foreach(CompletionsQueryData completionsData in completionsQueryData)
            {
                //IHtmlDocument completionReportPage = await OpenUri(CompletionReportParameters(completionsData.Columns.TrackingNo, api));

                directionalSurveyDocument = await OpenUri(DirectionalSurveyParameters(completionsData.Columns.TrackingNo));

                directionalSurveyPages = QueryParser.ParseDirectionalSurveyQuery(directionalSurveyDocument);

                DirectionalSurveyReport ds;

                foreach(DirectionalSurveyQueryData directionalSurveyPage in directionalSurveyPages)
                {
                    ds = new DirectionalSurveyReport(directionalSurveyPage.Columns.From, directionalSurveyPage.Columns.To);

                    directionalSurveyReport.Add(ds);
                }
            }

            //http://webapps.rrc.texas.gov/CMPL/viewPdfReportFormAction.do?method=cmplW2FormPdf&packetSummaryId=65016

            // id = viewer , class = pdfViewer

            return directionalSurveyReport;
        }

        public static async Task<string> CompletionReportG1QueryByApi(ApiNumber api)
        {
            return await CompletionReportG1QueryByApi(api, ScheduleType.Both);
        }

        public static async Task<string> CompletionReportG1QueryByApi(ApiNumber    api,
                                                                      ScheduleType scheduleType)
        {
            IHtmlDocument wellboreQueryResult = WellboreQueryByApi(api, scheduleType).Result;

            WellboreQueryData wellboreQueryData = QueryParser.ParseWellboreQuery(wellboreQueryResult).FirstOrDefault(wqd => wqd.Columns.OnSchedule == "Y");

            if(wellboreQueryData is null)
            {
                throw new Exception("wellboreQueryData is null");
            }

            IHtmlDocument completionsQueryResult = await OpenUri(wellboreQueryData.Columns.CompletionLink);

            List<CompletionsQueryData> completionsQueryData = QueryParser.ParseCompletionsQuery(completionsQueryResult);

            //IHtmlDocument? document = null;
            byte[]? response = null;

            //List<DirectionalSurveyQueryData> directionalSurveyPages;

            foreach(CompletionsQueryData completionsData in completionsQueryData)
            {
                try
                {
                    response =
                        await
                            client.GetByteArrayAsync($"http://webapps.rrc.texas.gov/CMPL/viewPdfReportFormAction.do?method=cmplG1FormPdf&packetSummaryId={completionsData.Columns.TrackingNo}");
                }
                catch(Exception)
                {
                    // ignored
                }

                if(response != null)
                {
                    break;
                }
            }

            string tempPath = @"D:\OilGasData"; //Path.GetTempPath();
            string path     = $"{api.ToString()}.G1.pdf";

            string tempFile = Path.Combine(tempPath, path);

            await File.WriteAllBytesAsync(tempFile, response);

            return tempFile;
        }

        public static async Task<string> CompletionReportsQueryByApi(ApiNumber api)
        {
            return await CompletionReportsQueryByApi(api, ScheduleType.Both);
        }

        private static async Task CompletionReportQueryByTrackingNumber(ApiNumber                  api,
                                                                        string                     reportType,
                                                                        List<CompletionsQueryData> completionsQueryData)
        {
            //List<DirectionalSurveyQueryData> directionalSurveyPages;
            byte[]? response = null;

            foreach(CompletionsQueryData completionsData in completionsQueryData)
            {
                try
                {
                    response =
                        await
                            client.GetByteArrayAsync($"http://webapps.rrc.texas.gov/CMPL/viewPdfReportFormAction.do?method=cmpl{reportType}FormPdf&packetSummaryId={completionsData.Columns.TrackingNo}");
                }
                catch(Exception)
                {
                    // ignored
                }

                if(response != null)
                {
                    break;
                }
            }

            if(response != null)
            {
                string tempPath = Path.GetTempPath();
                string path     = $"{api}.{reportType}.pdf";

                string tempFolder = Path.Combine(tempPath,   "CompletionReports");
                string tempFile   = Path.Combine(tempFolder, path);

                if(!Directory.Exists(tempFolder))
                {
                    Directory.CreateDirectory(tempFolder);
                }

                await File.WriteAllBytesAsync(tempFile, response);
            }
        }

        public static async Task<string> CompletionReportsQueryByApi(ApiNumber    api,
                                                                     ScheduleType scheduleType)
        {
            IHtmlDocument wellboreQueryResult = WellboreQueryByApi(api, scheduleType).Result;

            WellboreQueryData wellboreQueryData = QueryParser.ParseWellboreQuery(wellboreQueryResult).FirstOrDefault(wqd => wqd.Columns.OnSchedule == "Y");

            if(wellboreQueryData is null)
            {
                throw new Exception("wellboreQueryData is null");
            }

            IHtmlDocument completionsQueryResult = await OpenUri(wellboreQueryData.Columns.CompletionLink);

            List<CompletionsQueryData> completionsQueryData = QueryParser.ParseCompletionsQuery(completionsQueryResult);

            await CompletionReportQueryByTrackingNumber(api, CompletionReportType.W1, completionsQueryData);

            await CompletionReportQueryByTrackingNumber(api, CompletionReportType.W2, completionsQueryData);

            await CompletionReportQueryByTrackingNumber(api, CompletionReportType.W4, completionsQueryData);

            await CompletionReportQueryByTrackingNumber(api, CompletionReportType.W10, completionsQueryData);

            await CompletionReportQueryByTrackingNumber(api, CompletionReportType.W14, completionsQueryData);

            //await CompletionReportQueryByTrackingNumber(api, CompletionReportType.G1, completionsQueryData);

            await CompletionReportQueryByTrackingNumber(api, CompletionReportType.G5, completionsQueryData);

            await CompletionReportQueryByTrackingNumber(api, CompletionReportType.G10, completionsQueryData);

            await CompletionReportQueryByTrackingNumber(api, CompletionReportType.L1, completionsQueryData);

            await CompletionReportQueryByTrackingNumber(api, CompletionReportType.P1, completionsQueryData);

            await CompletionReportQueryByTrackingNumber(api, CompletionReportType.P4, completionsQueryData);

            await CompletionReportQueryByTrackingNumber(api, CompletionReportType.P6, completionsQueryData);

            await CompletionReportQueryByTrackingNumber(api, CompletionReportType.P15, completionsQueryData);

            return $"{api}.pdf";
        }

        public static async Task<GisQuery?> GisQueryByApi(ApiNumber           api,
                                                          GisCoordinateSystem coorSystem = GisCoordinateSystem.GCS_North_American_1983)
        {
            const string urlQuerty = "https://gis.rrc.texas.gov/server/rest/services/rrc_public/RRC_Public_Viewer_Srvs/MapServer/0/query?where=";

            int coorSystemValue = (int)coorSystem;

            string coorSystemStr = coorSystemValue < 10000 ? ((int)coorSystem).ToString("D4", CultureInfo.InvariantCulture) : ((int)coorSystem).ToString("D5", CultureInfo.InvariantCulture);

            string quertyArgs = $"API in ('{api.CountyCode}{api.UniqueWellIdentifier}')&outSR={coorSystemStr}&outFields=*&f=pjson";

            byte[] response = await client.GetByteArrayAsync(urlQuerty + quertyArgs);

            GisQuery? gisQuery = null;

            if(response != null)
            {
                gisQuery = GisQuery.FromJson(Encoding.Default.GetString(response));
            }

            return gisQuery;
        }

        //public static string CompletionReport()
        //{
        //    var configPath = Path.Combine("TestData", "support.office.com.json");
        //
        //    var config = StructuredDataConfig.ParseJsonFile(configPath);
        //
        //    var extractor = new StructuredDataExtractor(config);
        //
        //    var result = extractor.Extract(File.ReadAllText(Path.Combine("TestData", "support.office.com.html")));
        //
        //    var json = JsonConvert.SerializeObject(result, Formatting.Indented);
        //
        //    return string.Empty;
        //}

        //Dictionary<string, string> requestParams = new Dictionary<string, string>
        //{
        //    {
        //        "actionManager.recordCountHndlr.inputValue", "1"
        //    },
        //    {
        //        "actionManager.currentIndexHndlr.inputValue", "0"
        //    },
        //    {
        //        "actionManager.actionRcrd[0].actionDisplayNmHndlr.inputValue", "Search Criteria"
        //    },
        //    {
        //        "actionManager.actionRcrd[0].hostHndlr.inputValue", RrcStateTxBaseUrl + ":" + RrcStateTxPort
        //    },
        //    {
        //        "actionManager.actionRcrd[0].contextPathHndlr.inputValue", "/" + RrcStateTxSection
        //    },
        //    {
        //        "actionManager.actionRcrd[0].actionHndlr.inputValue", "/" + SpecificLeaseQuery
        //    },
        //    {
        //        "actionManager.actionRcrd[0].actionParameterHndlr.inputValue", "methodToCall"
        //    },
        //    {
        //        "actionManager.actionRcrd[0].actionMethodHndlr.inputValue", "unspecified"
        //    },
        //    {
        //        "actionManager.actionRcrd[0].pagerParameterKeyHndlr.inputValue", ""
        //    },
        //    {
        //        "actionManager.actionRcrd[0].actionParametersHndlr.inputValue", ""
        //    },
        //    {
        //        "actionManager.actionRcrd[0].returnIndexHndlr.inputValue", "0"
        //    },
        //    {
        //        "methodToCall", "generateSpecificLeaseCSVReport"
        //    },
        //    {
        //        "viewType", "init"
        //    },
        //    {
        //        "searchArgs.searchType", "specificLease"
        //    },
        //    {
        //        "searchType", "specificLease"
        //    },
        //    {
        //        "searchArgs.activeTabsFlagwordHndlr.inputValue", "0"
        //    },
        //    {
        //        "searchArgs.orderByHndlr.inputValue", ""
        //    },
        //    {
        //        "searchArgs.leaseNumberArg", leaseNumber
        //    },
        //    {
        //        "searchArgs.districtCodeArg", districtCode
        //    },
        //    {
        //        "searchArgs.oilOrGasArg", (leaseType == LeaseType.Oil) ? "O" : "G"
        //    },
        //    {
        //        "searchArgs.startMonthArg", "01"
        //    },
        //    {
        //        "searchArgs.startYearArg", "1993"
        //    },
        //    {
        //        "searchArgs.endMonthArg", DateTime.Now.Month.ToString("D2",
        //                                                              CultureInfo.InvariantCulture)
        //    },
        //    {
        //        "searchArgs.endYearArg", DateTime.Now.Year.ToString("D4",
        //                                                            CultureInfo.InvariantCulture)
        //    },
        //    {
        //        "submit", "Submit"
        //    }
        //};

        //public static object production_from_lease(string lease,
        //                                           string district,
        //                                           string well_type)
        //{
        //    object query_result = rrc_production_query(lease,
        //                                               district,
        //                                               well_type);
        //
        //    return parse_production_csv(query_result,
        //                                well_type);
        //}

        private static void SplitApiString(ReadOnlySpan<char> api,
                                           out string         prefix,
                                           out string         suffix)
        {
            ReadOnlySpan<char> pre = api.Slice(2, 3);

            ReadOnlySpan<char> suf = api.Slice(5, 5);

            prefix = pre.ToString();
            suffix = suf.ToString();
        }

        //public static object try_parse(object val,
        //                               object typ,
        //                               object @default)
        //{
        //    try
        //    {
        //        return typ(val);
        //    }
        //    catch(ValueError)
        //    {
        //        return @default;
        //    }
        //}
    }
}
