using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Net;
using System.Net.Http;
using System.Threading.Tasks;

using AngleSharp;
using AngleSharp.Html.Dom;
using AngleSharp.Html.Parser;

namespace OilGas.Data.FracFocus
{
    public class QueryBuilder
    {
        public const string BaseUrl = "https://fracfocusdata.org/DisclosureSearch/Search.aspx";

        private static readonly HttpClient client;

        private static readonly WebClient webClient;

        static QueryBuilder()
        {
            client = new HttpClient();

            webClient = new WebClient()
            {
                Credentials = CredentialCache.DefaultNetworkCredentials
            };
        }

        private static IHtmlDocument CreateDocument(string responseString)
        {
            IBrowsingContext context = BrowsingContext.New(Configuration.Default);

            HtmlParserOptions options = new HtmlParserOptions()
            {
                IsScripting = context != null && context.IsScripting()
            };

            IHtmlDocument document = new HtmlParser(options,
                                                    context).ParseDocument(responseString);

            return document;
        }

        public static async Task<IHtmlDocument> OpenUri(string uri)
        {
            string responseString = "HTTP request failed.";

            try
            {
                responseString = await client.GetStringAsync(uri);

                //StreamReader reader = new StreamReader(stream);
                //responseString = reader.ReadToEnd();
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


        public static async Task<IHtmlDocument> QueryByApi(ApiNumber api)
        {
            string pre = api.CountyCode.ToString();
            string suf = api.UniqueWellIdentifier.ToString();

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
    }
}