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

using Newtonsoft.Json;

using OilGas.Data;

using OpenScraping;
using OpenScraping.Config;

namespace OilGas.Data.RRC.Texas
{
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

        public static readonly Uri WellboreQueryAction = new Uri(RrcStateTx + WellboreQuery);

        public static readonly Uri ProductionQueryAction = new Uri(RrcStateTx + ProductionQuery);

        public static readonly Uri SpecificLeaseQueryAction = new Uri(RrcStateTx + SpecificLeaseQuery);

        public static readonly Uri OrganizationQueryAction = new Uri(RrcStateTx + OrganizationQuery);

        public static readonly Uri DrillingPermitsQueryAction = new Uri(RrcStateTx + DrillingPermitsQuery);

        // Completion http://webapps.rrc.texas.gov/CMPL/ewaSearchAction.do
        // Tracking No.: 54106
        // http://webapps.rrc.texas.gov/CMPL/viewPdfReportFormAction.do?method=cmplW2FormPdf&packetSummaryId=54106
        // ACID, FRACTURE, CEMENT SQUEEZE, CAST IRON BRIDGE PLUG, RETAINER, ETC.

        private static readonly CookieContainer   cookies;

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

            HtmlParserOptions options = new HtmlParserOptions
            {
                IsScripting = context != null && context.IsScripting()
            };

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
            Formatting               = Formatting.Indented,
            MetadataPropertyHandling = MetadataPropertyHandling.Ignore,
            DateParseHandling        = DateParseHandling.None
        };

        public static async Task<T> PerformQuery<T>(Uri                        requestUri,
                                                    Dictionary<string, string> request_params,
                                                    StructuredDataExtractor    dataExtractor)
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

            return JsonConvert.DeserializeObject<T>(JsonConvert.SerializeObject(dataExtractor.Extract(responseString), Formatting.Indented), settings);
        }

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
                {
                    "searchArgs.apiNoPrefixArg", pre
                },
                {
                    "searchArgs.apiNoSuffixArg", suf
                },
                {
                    "searchArgs.scheduleTypeArg", scheduleType
                },
                {
                    "methodToCall", "search"
                }
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
                {
                    "searchArgs.districtCodeArg", districtCode.ToString()
                },
                {
                    "searchArgs.wellTypeArg", wellType
                },
                {
                    "searchArgs.scheduleTypeArg", scheduleType
                },
                {
                    "methodToCall", "search"
                }
            };

            return await PerformQueryAndCreateDocument(WellboreQueryAction, requestParams);
        }

        public static async Task<IHtmlDocument> WellboreQueryByLeaseNumber(string leaseNumber)
        {
            return await WellboreQueryByLeaseNumber(leaseNumber, ScheduleType.Both);
        }

        public static async Task<IHtmlDocument> WellboreQueryByLeaseNumber(string       leaseNumber,
                                                                           ScheduleType scheduleType)
        {
            Dictionary<string, string> requestParams = new Dictionary<string, string>
            {
                {
                    "searchArgs.leaseNumberArg", leaseNumber
                },
                {
                    "searchArgs.scheduleTypeArg", scheduleType
                },
                {
                    "methodToCall", "toLeaseQuery"
                }
            };

            return await PerformQueryAndCreateDocument(WellboreQueryAction, requestParams);
        }

        public static async Task<IHtmlDocument> OrganizationNameQuery(string operatorNumber)
        {
            Dictionary<string, string> requestParams = new Dictionary<string, string>
            {
                {
                    "number", operatorNumber
                },
                {
                    "methodToCall", "searchByNumber"
                }
            };

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

        public static async Task<(string, Lease)> SpecificLeaseProductionQueryByApi(ApiNumber api)
        {
            return await SpecificLeaseProductionQueryByApi(api, ScheduleType.Both);
        }

        public static async Task<(string, Lease)> SpecificLeaseProductionQueryByApi(ApiNumber    api,
                                                                           ScheduleType scheduleType)
        {
            IHtmlDocument wellboreQueryResult = WellboreQueryByApi(api, scheduleType).Result;

            WellboreQueryData wellboreQueryData = QueryParser.ParseWellboreQuery(wellboreQueryResult).FirstOrDefault(wqd => wqd.Columns.OnSchedule == "Y");

            if(wellboreQueryData == null)
            {
                throw new Exception("wellboreQueryData == null");
            }

            LeaseDetailQueryData leaseDetailQueryData = await LeaseDetailQuery(wellboreQueryData);

            Lease lease = Lease.Create(wellboreQueryData, leaseDetailQueryData);

            return (await SpecificLeaseProductionQuery(lease), lease);
        }

        public static async Task<string> SpecificLeaseProductionQuery(Lease lease)
        {
            return await SpecificLeaseProductionQuery(lease.LeaseNumber, lease.DistrictCode, lease.LeaseType);
        }

        public static async Task<string> SpecificLeaseProductionQuery(string       leaseNumber,
                                                                      DistrictCode districtCode,
                                                                      LeaseType    leaseType)
        {
            Dictionary<string, string> requestParams = new Dictionary<string, string>
            {
                {
                    "actionManager.actionRcrd[0].actionDisplayNmHndlr.inputValue", "Search Criteria"
                },
                {
                    "actionManager.actionRcrd[0].actionHndlr.inputValue", "/specificLeaseQueryAction.do"
                },
                {
                    "actionManager.actionRcrd[0].actionMethodHndlr.inputValue", "unspecified"
                },
                {
                    "actionManager.actionRcrd[0].actionParameterHndlr.inputValue", "methodToCall"
                },
                {
                    "actionManager.actionRcrd[0].actionParametersHndlr.inputValue", ""
                },
                {
                    "actionManager.actionRcrd[0].contextPathHndlr.inputValue", "/EWA"
                },
                {
                    "actionManager.actionRcrd[0].hostHndlr.inputValue", "webapps2.rrc.state.tx.us:80"
                },
                {
                    "actionManager.actionRcrd[0].pagerParameterKeyHndlr.inputValue", ""
                },
                {
                    "actionManager.actionRcrd[0].returnIndexHndlr.inputValue", "0"
                },
                {
                    "actionManager.currentIndexHndlr.inputValue", "0"
                },
                {
                    "actionManager.recordCountHndlr.inputValue", "1"
                },
                {
                    "methodToCall", "generateSpecificLeaseCSVReport"
                },
                {
                    "searchArgs.activeTabsFlagwordHndlr.inputValue", "0"
                },
                {
                    "searchArgs.leaseNumberArg", leaseNumber
                },
                {
                    "searchArgs.districtCodeArg", districtCode.ToString()
                },
                {
                    "searchArgs.oilOrGasArg", leaseType == LeaseType.Oil ? "O" : "G"
                },
                {
                    "searchArgs.startMonthArg", "01"
                },
                {
                    "searchArgs.startYearArg", "1993"
                },
                {
                    "searchArgs.endMonthArg", DateTime.Now.Month.ToString("D2", CultureInfo.InvariantCulture)
                },
                {
                    "searchArgs.endYearArg", DateTime.Now.Year.ToString("D4", CultureInfo.InvariantCulture)
                },
                {
                    "searchArgs.orderByHndlr.inputValue", ""
                },
                {
                    "searchArgs.searchType", "specificLease"
                },
                {
                    "searchType", "specificLease"
                },
                {
                    "submit", "Submit"
                },
                {
                    "viewType", "init"
                }
            };

            // searchArgs.orderByColumnName	""
            // searchArgs.initialViewArgHndlr.inputValue	""
            // searchArgs.startMonthArgHndlr.inputValue	"01"
            // searchArgs.startYearArgHndlr.inputValue	"1993"
            // searchArgs.endMonthArgHndlr.inputValue	"01"
            // searchArgs.endYearArgHndlr.inputValue	"2020"
            // searchArgs.oilOrGasArgHndlr.inputValue	"G"
            // searchArgs.gasWellNoHndlr.inputValue	"01H"
            // searchArgs.searchTypeHndlr.inputValue	"specificLease"
            // searchArgs.viewTypeHndlr.inputValue	"prodAndTotalDisp"
            // searchArgs.activeTabsFlagwordHndlr.inputValue	"0"
            // searchArgs.orderByHndlr.inputValue	""
            // searchArgs.leaseTypeArgHndlr.inputValue	""
            // searchArgs.districtCodeArgHndlr.inputValue	"02"
            // searchArgs.leaseNumberArgHndlr.inputValue	"267697"
            // searchArgs.fieldNumbersArgHndlr.inputValue	""
            // searchArgs.fieldNamesHndlr.inputValue	""
            // searchArgs.operatorNumbersArgHndlr.inputValue	""
            // searchArgs.operatorNamesHndlr.inputValue	""
            // searchArgs.onShoreCountyCodeArgHndlr.inputValue	""
            // searchArgs.offShoreCountyCodeArgHndlr.inputValue	""
            // searchArgs.leaseNameArgHndlr.inputValue	"HOPE+SCHORLEMER+01"
            // searchArgs.geoRegionOptionHndlr.inputValue	"district"
            // searchArgs.statewideFlagHndlr.inputValue	""
            // methodToCall	"generateSpecificLeaseCSVReport"
            // actionManager.recordCountHndlr.inputValue	"2"
            // actionManager.currentIndexHndlr.inputValue	"1"
            // actionManager.actionRcrd[0].actionDisplayNmHndlr.inputValue	"Search+Criteria"
            // actionManager.actionRcrd[0].hostHndlr.inputValue	"webapps2.rrc.texas.gov:80"
            // actionManager.actionRcrd[0].contextPathHndlr.inputValue	"/EWA"
            // actionManager.actionRcrd[0].actionHndlr.inputValue	"/specificLeaseQueryAction.do"
            // actionManager.actionRcrd[0].actionParameterHndlr.inputValue	"methodToCall"
            // actionManager.actionRcrd[0].actionMethodHndlr.inputValue	"unspecified"
            // actionManager.actionRcrd[0].pagerParameterKeyHndlr.inputValue	""
            // actionManager.actionRcrd[0].actionParametersHndlr.inputValue	""
            // actionManager.actionRcrd[0].returnIndexHndlr.inputValue	"0"
            // actionManager.actionRcrd[0].argRcrdParameters(searchArgs.paramValue)	"|2=01|3=1993|4=01|5=2020|6=G|8=specificLease|9=prodAndTotalDisp|10=0|102=02|103=267697|203=HOPE+SCHORLEMER+01|204=district"
            // actionManager.actionRcrd[1].actionDisplayNmHndlr.inputValue	"District:+02"
            // actionManager.actionRcrd[1].hostHndlr.inputValue	"webapps2.rrc.texas.gov:80"
            // actionManager.actionRcrd[1].contextPathHndlr.inputValue	"/EWA"
            // actionManager.actionRcrd[1].actionHndlr.inputValue	"/specificLeaseQueryAction.do"
            // actionManager.actionRcrd[1].actionParameterHndlr.inputValue	"methodToCall"
            // actionManager.actionRcrd[1].actionMethodHndlr.inputValue	"search"
            // actionManager.actionRcrd[1].pagerParameterKeyHndlr.inputValue	"slPager.paramValue"
            // actionManager.actionRcrd[1].actionParametersHndlr.inputValue	""
            // actionManager.actionRcrd[1].returnIndexHndlr.inputValue	"0"
            // actionManager.actionRcrd[1].argRcrdParameters(searchArgs.paramValue)	"|2=01|3=1993|4=01|5=2020|6=G|7=01H|8=specificLease|9=prodAndTotalDisp|10=0|102=02|103=267697|203=HOPE+SCHORLEMER+01|204=district"
            // actionManager.actionRcrd[1].argRcrdParameters(slPager.paramValue)	"|1=1|2=10|3=103|4=0|5=11|6=10"
            // searchArgs.startMonthArg	"01"
            // searchArgs.startYearArg	"1993"
            // searchArgs.endMonthArg	"01"
            // searchArgs.endYearArg	"2020"
            // pager.pageSize	"10"


            // http://webapps2.rrc.texas.gov/EWA/specificLeaseQueryAction.do?searchArgs.orderByColumnName=&searchArgs.initialViewArgHndlr.inputValue=&searchArgs.startMonthArgHndlr.inputValue=01&searchArgs.startYearArgHndlr.inputValue=1993&searchArgs.endMonthArgHndlr.inputValue=01&searchArgs.endYearArgHndlr.inputValue=2020&searchArgs.oilOrGasArgHndlr.inputValue=G&searchArgs.gasWellNoHndlr.inputValue=01H&searchArgs.searchTypeHndlr.inputValue=specificLease&searchArgs.viewTypeHndlr.inputValue=prodAndTotalDisp&searchArgs.activeTabsFlagwordHndlr.inputValue=0&searchArgs.orderByHndlr.inputValue=&searchArgs.leaseTypeArgHndlr.inputValue=&searchArgs.districtCodeArgHndlr.inputValue=02&searchArgs.leaseNumberArgHndlr.inputValue=267697&searchArgs.fieldNumbersArgHndlr.inputValue=&searchArgs.fieldNamesHndlr.inputValue=&searchArgs.operatorNumbersArgHndlr.inputValue=&searchArgs.operatorNamesHndlr.inputValue=&searchArgs.onShoreCountyCodeArgHndlr.inputValue=&searchArgs.offShoreCountyCodeArgHndlr.inputValue=&searchArgs.leaseNameArgHndlr.inputValue=HOPE+SCHORLEMER+01&searchArgs.geoRegionOptionHndlr.inputValue=district&searchArgs.statewideFlagHndlr.inputValue=&methodToCall=generateSpecificLeaseCSVReport&actionManager.recordCountHndlr.inputValue=2&actionManager.currentIndexHndlr.inputValue=1&actionManager.actionRcrd%5B0%5D.actionDisplayNmHndlr.inputValue=Search+Criteria&actionManager.actionRcrd%5B0%5D.hostHndlr.inputValue=webapps2.rrc.texas.gov%3A80&actionManager.actionRcrd%5B0%5D.contextPathHndlr.inputValue=%2FEWA&actionManager.actionRcrd%5B0%5D.actionHndlr.inputValue=%2FspecificLeaseQueryAction.do&actionManager.actionRcrd%5B0%5D.actionParameterHndlr.inputValue=methodToCall&actionManager.actionRcrd%5B0%5D.actionMethodHndlr.inputValue=unspecified&actionManager.actionRcrd%5B0%5D.pagerParameterKeyHndlr.inputValue=&actionManager.actionRcrd%5B0%5D.actionParametersHndlr.inputValue=&actionManager.actionRcrd%5B0%5D.returnIndexHndlr.inputValue=0&actionManager.actionRcrd%5B0%5D.argRcrdParameters%28searchArgs.paramValue%29=%7C2%3D01%7C3%3D1993%7C4%3D01%7C5%3D2020%7C6%3DG%7C8%3DspecificLease%7C9%3DprodAndTotalDisp%7C10%3D0%7C102%3D02%7C103%3D267697%7C203%3DHOPE%2BSCHORLEMER%2B01%7C204%3Ddistrict&actionManager.actionRcrd%5B1%5D.actionDisplayNmHndlr.inputValue=District%3A+02&actionManager.actionRcrd%5B1%5D.hostHndlr.inputValue=webapps2.rrc.texas.gov%3A80&actionManager.actionRcrd%5B1%5D.contextPathHndlr.inputValue=%2FEWA&actionManager.actionRcrd%5B1%5D.actionHndlr.inputValue=%2FspecificLeaseQueryAction.do&actionManager.actionRcrd%5B1%5D.actionParameterHndlr.inputValue=methodToCall&actionManager.actionRcrd%5B1%5D.actionMethodHndlr.inputValue=search&actionManager.actionRcrd%5B1%5D.pagerParameterKeyHndlr.inputValue=slPager.paramValue&actionManager.actionRcrd%5B1%5D.actionParametersHndlr.inputValue=&actionManager.actionRcrd%5B1%5D.returnIndexHndlr.inputValue=0&actionManager.actionRcrd%5B1%5D.argRcrdParameters%28searchArgs.paramValue%29=%7C2%3D01%7C3%3D1993%7C4%3D01%7C5%3D2020%7C6%3DG%7C7%3D01H%7C8%3DspecificLease%7C9%3DprodAndTotalDisp%7C10%3D0%7C102%3D02%7C103%3D267697%7C203%3DHOPE%2BSCHORLEMER%2B01%7C204%3Ddistrict&actionManager.actionRcrd%5B1%5D.argRcrdParameters%28slPager.paramValue%29=%7C1%3D1%7C2%3D10%7C3%3D103%7C4%3D0%7C5%3D11%7C6%3D10&searchArgs.startMonthArg=01&searchArgs.startYearArg=1993&searchArgs.endMonthArg=01&searchArgs.endYearArg=2020&pager.pageSize=10
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
                {
                    "methodToCall", "search"
                },
                {
                    "searchArgs.permitStatusNoHndlr.inputValue", ""
                },
                {
                    "searchArgs.apiNoHndlr.inputValue", ""
                },
                {
                    keyName, keyValue
                },
                {
                    "searchArgs.npzFlagHndlr.inputValue", ""
                },
                {
                    "searchArgs.offLeaseSurfLocFlagHndlr.inputValue", ""
                },
                {
                    "searchArgs.offLeasePntrnPtFlagHndlr.inputValue", ""
                },
                {
                    "searchArgs.operatorNameWildcardHndlr.inputValue", "beginsWith"
                },
                {
                    "searchArgs.operatorNameHndlr.inputValue", ""
                },
                {
                    "searchArgs.operatorNoHndlr.inputValue", ""
                },
                {
                    "searchArgs.leaseNameWildcardHndlr.inputValue", "beginsWith"
                },
                {
                    "searchArgs.leaseNameHndlr.inputValue", ""
                },
                {
                    "searchArgs.leaseNoHndlr.inputValue", ""
                },
                {
                    "searchArgs.wellNoHndlr.inputValue", ""
                },
                {
                    "searchArgs.fieldNameWildcardHndlr.inputValue", "beginsWith"
                },
                {
                    "searchArgs.fieldNameHndlr.inputValue", ""
                },
                {
                    "searchArgs.fieldNoHndlr.inputValue", ""
                },
                {
                    "searchArgs.surveyNameWildcardHndlr.inputValue", "beginsWith"
                },
                {
                    "searchArgs.surveyNameHndlr.inputValue", ""
                },
                {
                    "searchArgs.wellTypeCodeHndlr.inputValue", ""
                },
                {
                    "searchArgs.totalDepthHndlr.inputValue", ""
                },
                {
                    "searchArgs.filingPurposeCodeHndlr.inputValue", ""
                },
                {
                    "searchArgs.ammendmentFlagHndlr.inputValue", ""
                },
                {
                    "searchArgs.statusCodeHndlr.inputValue", ""
                },
                {
                    "searchArgs.wellboreProfileCodeHndlr.inputValue", ""
                },
                {
                    "searchArgs.wellLocationCodeHndlr.inputValue", ""
                },
                {
                    "searchArgs.completionStatusCodeHndlr.inputValue", ""
                },
                {
                    "searchArgs.psaFlagHndlr.inputValue", ""
                },
                {
                    "searchArgs.allocationFlagHndlr.inputValue", ""
                },
                {
                    "searchArgs.stackedLateralFlagHndlr.inputValue", ""
                },
                {
                    "searchArgs.approvedDtFromHndlr.inputValue", $"{fromMonth}/{fromDay}/{fromYear}"
                },
                {
                    "searchArgs.approvedDtToHndlr.inputValue", $"{toMonth}/{toDay}/{toYear}"
                },
                {
                    "searchArgs.submittedDtFromHndlr.inputValue", ""
                },
                {
                    "searchArgs.submittedDtToHndlr.inputValue", ""
                }
            };

            IHtmlDocument document = await PerformQueryAndCreateDocument(DrillingPermitsQueryAction, requestParams);

            Dictionary<string, string> requestParams2 = new Dictionary<string, string>
            {
                {
                    "searchArgs.orderByColumnName", ""
                },
                {
                    "searchArgs.orderByHndlr.inputValue", ""
                },
                {
                    "searchArgs.universalDocNoHndlr.inputValue", ""
                },
                {
                    "searchArgs.permitStatusNoHndlr.inputValue", ""
                },
                {
                    "searchArgs.apiNoHndlr.inputValue", ""
                },
                {
                    "searchArgs.districtCodeHndlr.inputValue", (districtCode != null ? $"{(int)districtCode}" : "")
                },
                {
                    "searchArgs.countyCodeHndlr.inputValue", (county != null ? $"{(int)county}" : "")
                },
                {
                    "searchArgs.operatorNameWildcardHndlr.inputValue", "beginsWith"
                },
                {
                    "searchArgs.operatorNameHndlr.inputValue", ""
                },
                {
                    "searchArgs.operatorNoHndlr.inputValue", ""
                },
                {
                    "searchArgs.leaseNameWildcardHndlr.inputValue", "beginsWith"
                },
                {
                    "searchArgs.leaseNameHndlr.inputValue", ""
                },
                {
                    "searchArgs.leaseNoHndlr.inputValue", ""
                },
                {
                    "searchArgs.wellNoHndlr.inputValue", ""
                },
                {
                    "searchArgs.fieldNameWildcardHndlr.inputValue", "beginsWith"
                },
                {
                    "searchArgs.fieldNameHndlr.inputValue", ""
                },
                {
                    "searchArgs.fieldNoHndlr.inputValue", ""
                },
                {
                    "searchArgs.surveyNameWildcardHndlr.inputValue", "beginsWith"
                },
                {
                    "searchArgs.surveyNameHndlr.inputValue", ""
                },
                {
                    "searchArgs.wellTypeCodeHndlr.inputValue", ""
                },
                {
                    "searchArgs.totalDepthHndlr.inputValue", ""
                },
                {
                    "searchArgs.filingPurposeCodeHndlr.inputValue", ""
                },
                {
                    "searchArgs.statusCodeHndlr.inputValue", ""
                },
                {
                    "searchArgs.wellboreProfileCodeHndlr.inputValue", ""
                },
                {
                    "searchArgs.wellLocationCodeHndlr.inputValue", ""
                },
                {
                    "searchArgs.completionStatusCodeHndlr.inputValue", ""
                },
                {
                    "searchArgs.approvedDtFromHndlr.inputValue", $"{fromMonth}/{fromDay}/{fromYear}"
                },
                {
                    "searchArgs.approvedDtToHndlr.inputValue", $"{toMonth}/{toDay}/{toYear}"
                },
                {
                    "searchArgs.submittedDtFromHndlr.inputValue", ""
                },
                {
                    "searchArgs.submittedDtToHndlr.inputValue", ""
                },
                {
                    "searchArgs.npzFlagHndlr.inputValue", ""
                },
                {
                    "searchArgs.ammendmentFlagHndlr.inputValue", ""
                },
                {
                    "searchArgs.swrExceptionCodeHndlr.inputValue", ""
                },
                {
                    "searchArgs.psaFlagHndlr.inputValue", ""
                },
                {
                    "searchArgs.allocationFlagHndlr.inputValue", ""
                },
                {
                    "searchArgs.stackedLateralFlagHndlr.inputValue", ""
                },
                {
                    "searchArgs.offLeasePntrnPtFlagHndlr.inputValue", ""
                },
                {
                    "searchArgs.offLeaseSurfLocFlagHndlr.inputValue", ""
                },
                {
                    "methodToCall", "genDrillingPermitsCSVRpt"
                },
                {
                    "actionManager.recordCountHndlr.inputValue", "2"
                },
                {
                    "actionManager.currentIndexHndlr.inputValue", "1"
                },
                {
                    "actionManager.actionRcrd[0].actionDisplayNmHndlr.inputValue", ""
                },
                {
                    "actionManager.actionRcrd[0].hostHndlr.inputValue", "webapps2.rrc.texas.gov:80"
                },
                {
                    "actionManager.actionRcrd[0].contextPathHndlr.inputValue", "/EWA"
                },
                {
                    "actionManager.actionRcrd[0].actionHndlr.inputValue", "/drillingPermitsQueryAction.do"
                },
                {
                    "actionManager.actionRcrd[0].actionParameterHndlr.inputValue", "methodToCall"
                },
                {
                    "actionManager.actionRcrd[0].actionMethodHndlr.inputValue", "returnToSearch"
                },
                {
                    "actionManager.actionRcrd[0].pagerParameterKeyHndlr.inputValue", ""
                },
                {
                    "actionManager.actionRcrd[0].actionParametersHndlr.inputValue", ""
                },
                {
                    "actionManager.actionRcrd[0].returnIndexHndlr.inputValue", "0"
                },
                {
                    "actionManager.actionRcrd[0].argRcrdParameters(searchArgs.paramValue)",
                    $"|{(county != null ? $"1004={(int)county}" : $"1003={(int)districtCode}")}|1005=beginsWith|1008=beginsWith|1012=beginsWith|1015=beginsWith|1024={fromMonth}/{fromDay}/{fromYear}|1025={toMonth}/{toDay}/{toYear}"
                },
                {
                    "actionManager.actionRcrd[1].actionDisplayNmHndlr.inputValue", ""
                },
                {
                    "actionManager.actionRcrd[1].hostHndlr.inputValue", "webapps2.rrc.texas.gov:80"
                },
                {
                    "actionManager.actionRcrd[1].contextPathHndlr.inputValue", "/EWA"
                },
                {
                    "actionManager.actionRcrd[1].actionHndlr.inputValue", "/drillingPermitsQueryAction.do"
                },
                {
                    "actionManager.actionRcrd[1].actionParameterHndlr.inputValue", "methodToCall"
                },
                {
                    "actionManager.actionRcrd[1].actionMethodHndlr.inputValue", "search"
                },
                {
                    "actionManager.actionRcrd[1].pagerParameterKeyHndlr.inputValue", "pager.paramValue"
                },
                {
                    "actionManager.actionRcrd[1].actionParametersHndlr.inputValue", ""
                },
                {
                    "actionManager.actionRcrd[1].returnIndexHndlr.inputValue", "0"
                },
                {
                    "actionManager.actionRcrd[1].argRcrdParameters(pager.paramValue)", "|1=1|2=10|3=6668|4=0|5=667|6=10"
                },
                {
                    "actionManager.actionRcrd[1].argRcrdParameters(searchArgs.paramValue)",
                    $"|{(county != null ? $"1004={(int)county}" : $"1003={(int)districtCode}")}|1005=beginsWith|1008=beginsWith|1012=beginsWith|1015=beginsWith|1024={fromMonth}/{fromDay}/{fromYear}|1025={toMonth}/{toDay}/{toYear}"
                },
                {
                    "pager.pageSize", "10"
                },
                {
                    "propertyValue", ""
                }
            };

            StringBuilder sb = new StringBuilder();

            sb.Append("?");

            foreach ((string key, string value) in requestParams2)
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

            return await client.GetStringAsync(DrillingPermitsQueryAction + payload);
        }

        public static IEnumerable<string> OrganizationNameQueryByNumber(string operatorNumber)
        {
            return QueryParser.ParseOrganizationNameQuery(OrganizationNameQuery(operatorNumber).Result);
        }

        public static string CompletionReport()
        {
            var configPath = Path.Combine("TestData", "support.office.com.json");

            var config = StructuredDataConfig.ParseJsonFile(configPath);

            var extractor = new StructuredDataExtractor(config);

            var result = extractor.Extract(File.ReadAllText(Path.Combine("TestData", "support.office.com.html")));

            var json = JsonConvert.SerializeObject(result, Formatting.Indented);

            return string.Empty;
        }

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
