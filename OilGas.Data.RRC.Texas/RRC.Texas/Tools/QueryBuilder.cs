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
using AngleSharp.Html.Dom;
using AngleSharp.Html.Parser;

using Newtonsoft.Json;

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

        public const string WellboreQuery      = "wellboreQueryAction.do";
        public const string ProductionQuery    = "productionQueryAction.do";
        public const string SpecificLeaseQuery = "specificLeaseQueryAction.do";
        public const string OrganizationQuery  = "organizationQueryAction.do";

        public static readonly Uri WellboreQueryAction = new Uri(RrcStateTx + WellboreQuery);

        public static readonly Uri ProductionQueryAction = new Uri(RrcStateTx + ProductionQuery);

        public static readonly Uri SpecificLeaseQueryAction = new Uri(RrcStateTx + SpecificLeaseQuery);

        public static readonly Uri OrganizationQueryAction = new Uri(RrcStateTx + OrganizationQuery);

        // Completion http://webapps.rrc.texas.gov/CMPL/ewaSearchAction.do
        // Tracking No.: 54106
        // http://webapps.rrc.texas.gov/CMPL/viewPdfReportFormAction.do?method=cmplW2FormPdf&packetSummaryId=54106
        // ACID, FRACTURE, CEMENT SQUEEZE, CAST IRON BRIDGE PLUG, RETAINER, ETC.

        private static readonly HttpClient client;

        private static readonly WebClient webClient;

        static QueryBuilder()
        {
            //AppContext.SetSwitch("System.Net.Http.UseSocketsHttpHandler", true);

            //HttpClientHandler httpClientHandler = new HttpClientHandler()
            //{
            //    //Proxy           = new WebProxy(ProxyUrl, false),
            //    PreAuthenticate = true,
            //    Credentials     = CredentialCache.DefaultNetworkCredentials,

            //    //UseDefaultCredentials = true,
            //    AllowAutoRedirect = false
            //    //CookieContainer   = new CookieContainer()
            //};

            //client = new HttpClient(httpClientHandler,
            //                        true)
            //{
            //    Timeout = new TimeSpan(0,
            //                           0,
            //                           0,
            //                           0,
            //                           1000)
            //};

            client = new HttpClient();

            webClient = new WebClient()
            {
                //Proxy       = new WebProxy(ProxyUrl, false),
                Credentials = CredentialCache.DefaultNetworkCredentials
            };
        }

        //private static void GET()
        //{
        //    //var responseString = await client.GetStringAsync("http://www.example.com/recepticle.aspx");
        //}

        private static IHtmlDocument CreateDocument(string responseString)
        {
            IConfiguration config = Configuration.Default;

            IBrowsingContext context = BrowsingContext.New(config);

            HtmlParserOptions options = new HtmlParserOptions()
            {
                IsScripting = context != null && context.IsScripting()
            };

            HtmlParser parser = new HtmlParser(options,
                                               context);

            IHtmlDocument document = parser.ParseDocument(responseString);

            return document;
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

                Console.WriteLine("Message :{0} ",
                                  e.Message);
            }

            return CreateDocument(responseString);
        }

        public static async Task<IHtmlDocument> PerformQuery(Uri                        requestUri,
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

                HttpResponseMessage response = await client.PostAsync(requestUri,
                                                                      content);

                responseString = await response.Content.ReadAsStringAsync();
            }
            catch(HttpRequestException e)
            {
                Console.WriteLine("\nException Caught!");

                Console.WriteLine("Message :{0} ",
                                  e.Message);
            }

            return CreateDocument(responseString);
        }

        public static async Task<string> PerformCsvQuery(Uri                        requestUri,
                                                         Dictionary<string, string> request_params)
        {
            string responseString = "HTTP request failed.";

            try
            {
                FormUrlEncodedContent content = new FormUrlEncodedContent(request_params);
                //content.Headers.ContentType = new MediaTypeHeaderValue("application/x-www-form-urlencoded;charset=utf-8");

                //var buffer      = System.Text.Encoding.UTF8.GetBytes(myContent);
                //var byteContent = new ByteArrayContent(buffer);

                HttpResponseMessage response = await client.PostAsync(requestUri,
                                                                      content);

                responseString = await response.Content.ReadAsStringAsync();
            }
            catch(HttpRequestException e)
            {
                Console.WriteLine("\nException Caught!");

                Console.WriteLine("Message :{0} ",
                                  e.Message);
            }

            int indexOfTotal = responseString.IndexOf("Total",
                                                      StringComparison.Ordinal);

            return responseString.Substring(0,
                                            indexOfTotal);
        }

        public static async Task<IHtmlDocument> WellboreQueryByApi(ApiNumber api)
        {
            return await WellboreQueryByApi(api,
                                            ScheduleType.Both);
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

            return await PerformQuery(WellboreQueryAction,
                                      requestParams);
        }

        public static async Task<IHtmlDocument> WellboreQueryByWellType(WellType     wellType,
                                                                        DistrictCode districtCode)

        {
            return await WellboreQueryByWellType(wellType,
                                                 districtCode,
                                                 ScheduleType.Both);
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

            return await PerformQuery(WellboreQueryAction,
                                      requestParams);
        }

        public static async Task<IHtmlDocument> WellboreQueryByLeaseNumber(string leaseNumber)
        {
            return await WellboreQueryByLeaseNumber(leaseNumber,
                                                    ScheduleType.Both);
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

            return await PerformQuery(WellboreQueryAction,
                                      requestParams);
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

            return await PerformQuery(OrganizationQueryAction,
                                      requestParams);
        }

        public static async Task<LeaseDetailQueryData> LeaseDetailQuery(WellboreQueryData wellboreQueryData)
        {
            IHtmlDocument leaseDetailResult = await OpenUri(RrcStateTx + wellboreQueryData.LeaseDetailAction);

            LeaseDetailQueryData leaseDetailQueryData = QueryParser.ParseLeaseDetailQuery(leaseDetailResult);

            return leaseDetailQueryData;
        }

        public static async Task<string> SpecificLeaseProductionQueryByApi(ApiNumber api)
        {
            return await SpecificLeaseProductionQueryByApi(api,
                                                           ScheduleType.Both);
        }

        public static async Task<string> SpecificLeaseProductionQueryByApi(ApiNumber    api,
                                                                           ScheduleType scheduleType)
        {
            IHtmlDocument wellboreQueryResult = WellboreQueryByApi(api,
                                                                   scheduleType).Result;

            WellboreQueryData wellboreQueryData = QueryParser.ParseWellboreQuery(wellboreQueryResult).FirstOrDefault();

            if(wellboreQueryData == null)
            {
                throw new Exception("wellboreQueryData == null");
            }

            LeaseDetailQueryData leaseDetailQueryData = await LeaseDetailQuery(wellboreQueryData);

            Lease lease = Lease.Create(wellboreQueryData,
                                       leaseDetailQueryData);

            return await SpecificLeaseProductionQuery(lease);
        }

        public static async Task<string> SpecificLeaseProductionQuery(Lease lease)
        {
            return await SpecificLeaseProductionQuery(lease.Number,
                                                      lease.DistrictCode,
                                                      lease.LeaseType);
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
                    "searchArgs.endMonthArg", DateTime.Now.Month.ToString("D2",
                                                                          CultureInfo.InvariantCulture)
                },
                {
                    "searchArgs.endYearArg", DateTime.Now.Year.ToString("D4",
                                                                        CultureInfo.InvariantCulture)
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

            return await PerformCsvQuery(SpecificLeaseQueryAction,
                                         requestParams);
        }




        public static IEnumerable<string> OrganizationNameQueryByNumber(string operatorNumber)
        {
            return QueryParser.ParseOrganizationNameQuery(OrganizationNameQuery(operatorNumber).Result);
        }


        public static string CompletionReport()
        {

            var configPath = Path.Combine("TestData", "support.office.com.json");

            var config     = StructuredDataConfig.ParseJsonFile(configPath);

            var extractor  = new StructuredDataExtractor(config);

            var result     = extractor.Extract(File.ReadAllText(Path.Combine("TestData", "support.office.com.html")));

            var json       = JsonConvert.SerializeObject(result, Formatting.Indented);




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
            ReadOnlySpan<char> pre = api.Slice(2,
                                               3);

            ReadOnlySpan<char> suf = api.Slice(5,
                                               5);

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