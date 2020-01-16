// ReSharper disable InconsistentNaming

using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net.Http;
using System.Threading.Tasks;

using AngleSharp.Dom;

namespace OilGas.Data
{
    public class HtmlTag : IEquatable<HtmlTag>
    {
        public string Tag { get; }

        public HtmlTag Parent { get; }

        public HtmlTag Child { get; }

        public bool HasChild { get { return Child != null; } }

        public int Index { get; }

        public bool HasIndex { get { return Index > 0; } }

        #region Tags

        public HtmlTag body
        {
            get
            {
                return new HtmlTag(Parent,
                                   Tag,
                                   HtmlTags.body.ToString(),
                                   Index).Child;
            }
        }

        public HtmlTag href
        {
            get
            {
                return new HtmlTag(Parent,
                                   Tag,
                                   HtmlTags.href.ToString(),
                                   Index).Child;
            }
        }

        public HtmlTag text
        {
            get
            {
                return new HtmlTag(Parent,
                                   Tag,
                                   HtmlTags.text.ToString(),
                                   Index).Child;
            }
        }

        public HtmlTag @class
        {
            get
            {
                return new HtmlTag(Parent,
                                   Tag,
                                   HtmlTags.@class.ToString(),
                                   Index).Child;
            }
        }

        public HtmlTag dl
        {
            get
            {
                return new HtmlTag(Parent,
                                   Tag,
                                   HtmlTags.dl.ToString(),
                                   Index).Child;
            }
        }

        public HtmlTag dt
        {
            get
            {
                return new HtmlTag(Parent,
                                   Tag,
                                   HtmlTags.dt.ToString(),
                                   Index).Child;
            }
        }

        public HtmlTag a
        {
            get
            {
                return new HtmlTag(Parent,
                                   Tag,
                                   HtmlTags.a.ToString(),
                                   Index).Child;
            }
        }

        public HtmlTag table
        {
            get
            {
                return new HtmlTag(Parent,
                                   Tag,
                                   HtmlTags.table.ToString(),
                                   Index).Child;
            }
        }

        public HtmlTag tbody
        {
            get
            {
                return new HtmlTag(Parent,
                                   Tag,
                                   HtmlTags.tbody.ToString(),
                                   Index).Child;
            }
        }

        public HtmlTag tr
        {
            get
            {
                return new HtmlTag(Parent,
                                   Tag,
                                   HtmlTags.tr.ToString(),
                                   Index).Child;
            }
        }

        public HtmlTag td
        {
            get
            {
                return new HtmlTag(Parent,
                                   Tag,
                                   HtmlTags.td.ToString(),
                                   Index).Child;
            }
        }

        #endregion

        public HtmlTag(string tag)
        {
            Tag    = tag;
            Parent = null;
            Child  = null;

            Index = 0;
        }

        public HtmlTag(HtmlTag parent,
                       string  tag)
        {
            Tag    = tag;
            Parent = parent;
            Child  = null;

            Index = 0;
        }

        public HtmlTag(HtmlTag parent,
                       string  tag,
                       string  child,
                       int     index = 0)
        {
            Tag = tag;

            Parent = parent == null
                         ? null
                         : new HtmlTag(parent.Parent,
                                       parent.Tag,
                                       this,
                                       parent.Index);

            Child = new HtmlTag(this,
                                child);

            Index = index;
        }

        public HtmlTag(HtmlTag parent,
                       string  tag,
                       HtmlTag child,
                       int     index = 0)
        {
            Tag = tag;

            Parent = parent == null
                         ? null
                         : new HtmlTag(parent.Parent,
                                       parent.Tag,
                                       this,
                                       parent.Index);

            Child = child;

            Index = index;
        }

        //public HtmlTag(HtmlTag parent,
        //               string  child)
        //{
        //    Tag   = parent.Tag;
        //    return new HtmlTag(Parent, Tag, new HtmlTag(this, child);
        //    Index = 0;
        //}

        public HtmlTag GetRootParent()
        {
            if(Parent == null)
            {
                return this;
            }

            return Parent.GetRootParent();
        }

        public HtmlTag this[int index]
        {
            get
            {
                return new HtmlTag(Parent,
                                   Tag,
                                   Child,
                                   index);
            }
        }

        //public static implicit operator string(HtmlTag tag)
        //{
        //    return tag.ToString();
        //}

        public override string ToString()
        {
            return Tag;
        }

        public bool Equals(HtmlTag other)
        {
            if(ReferenceEquals(null,
                               other))
            {
                return false;
            }

            if(ReferenceEquals(this,
                               other))
            {
                return true;
            }

            return Tag == other.Tag &&
                   Equals(Parent,
                          other.Parent) &&
                   Equals(Child,
                          other.Child) &&
                   Index == other.Index;
        }

        public override bool Equals(object obj)
        {
            if(ReferenceEquals(null,
                               obj))
            {
                return false;
            }

            if(ReferenceEquals(this,
                               obj))
            {
                return true;
            }

            if(obj.GetType() != this.GetType())
            {
                return false;
            }

            return Equals((HtmlTag)obj);
        }

        public override int GetHashCode()
        {
            return HashCode.Combine(Tag,
                                    Parent,
                                    Child,
                                    Index);
        }

        public static bool operator ==(HtmlTag left,
                                       HtmlTag right)
        {
            return Equals(left,
                          right);
        }

        public static bool operator !=(HtmlTag left,
                                       HtmlTag right)
        {
            return !Equals(left,
                           right);
        }
    }

    public static class HtmlTags
    {
        public static readonly HtmlTag html   = new HtmlTag("html");
        public static readonly HtmlTag body   = new HtmlTag("body");
        public static readonly HtmlTag href   = new HtmlTag("href");
        public static readonly HtmlTag text   = new HtmlTag("#text");
        public static readonly HtmlTag @class = new HtmlTag("class");
        public static readonly HtmlTag dl     = new HtmlTag("dl");
        public static readonly HtmlTag dt     = new HtmlTag("dt");
        public static readonly HtmlTag a      = new HtmlTag("a");
        public static readonly HtmlTag table  = new HtmlTag("table");
        public static readonly HtmlTag tbody  = new HtmlTag("tbody");
        public static readonly HtmlTag tr     = new HtmlTag("tr");
        public static readonly HtmlTag td     = new HtmlTag("td");
        public static readonly HtmlTag strong = new HtmlTag("strong");
    }

    public static class HtmlExtensions
    {
        public static Stream GetStream(HttpClient httpClient,
                                       string     url)
        {
            Task<Stream> stream = GetStreamTask(httpClient,
                                                url);

            if(stream.Exception != null && stream.Exception.HResult != 0)
            {
                return null;
            }

            return stream.Result;
        }

        public static async Task<Stream> GetStreamTask(HttpClient httpClient,
                                                       string     url)
        {
            Stream stream = null;

            //HttpResponseMessage response = await HttpClient.GetAsync(url);

            if(MakeRequest(httpClient,
                           new Uri(url),
                           out HttpResponseMessage response))
            {
                stream = await response.Content.ReadAsStreamAsync();
            }

            return stream;
        }

        public static bool MakeRequest(HttpClient              httpClient,
                                       Uri                     url,
                                       out HttpResponseMessage responseMessage)
        {
            Task<HttpResponseMessage> taskResponse = httpClient.GetAsync(url);

            if(taskResponse?.Result == null)
            {
                throw new Exception();
            }

            HttpResponseMessage response = taskResponse.Result;

            int statusCode = (int)response.StatusCode;

            if(statusCode >= 300 && statusCode <= 399)
            {
                Uri redirectUri = response.Headers.Location;

                if(redirectUri != url)
                {
                    if(!redirectUri.IsAbsoluteUri)
                    {
                        redirectUri = new Uri(url.GetLeftPart(UriPartial.Authority) + redirectUri);
                    }

                    return MakeRequest(httpClient,
                                       redirectUri,
                                       out responseMessage);
                }
            }

            if(!response.IsSuccessStatusCode)
            {
                throw new Exception();
            }

            responseMessage = response;

            return true;
        }

        #region Getters

        public static string GetAttribute(this INode node)
        {
            return ((IElement)node)?.GetAttribute(HtmlTags.href.ToString());
        }

        public static IElement GetElementWithClass(this IElement node,
                                                   string        className)
        {
            if(node != null)
            {
                string attributeValue;

                if(node.Attributes.Length > 0)
                {
                    attributeValue = node.GetAttribute(HtmlTags.@class.ToString());

                    if(!string.IsNullOrEmpty(attributeValue) && attributeValue.Contains(className))
                    {
                        return node;
                    }
                }

                if(node.HasChildNodes)
                {
                    IHtmlCollection<IElement> descendantNodes     = node.Children; //.Where(child => child.Name != HtmlTags.text);
                    IElement                  foundDescendantNode = null;

                    Parallel.ForEach(descendantNodes,
                                     (descendantNode,
                                      loopState) =>
                                     {
                                         IElement descendantFoundNode = descendantNode.GetElementWithClass(className);

                                         if(descendantFoundNode != null)
                                         {
                                             foundDescendantNode = descendantFoundNode;
                                             loopState.Stop();
                                         }
                                     });

                    if(foundDescendantNode != null)
                    {
                        return foundDescendantNode;
                    }

                    //foreach(IElement descendantNode in descendantNodes)
                    //{
                    //    IElement descendantFoundNode = GetElementWithClass(descendantNode, className);

                    //    if(descendantFoundNode != null)
                    //    {
                    //        return descendantFoundNode;
                    //    }
                    //}
                }
            }

            return null;
        }

        public static IElement GetElementByTags(this IElement node,
                                                HtmlTag       tag)
        {
            if(node.LocalName != tag.ToString())
            {
                throw new Exception();
            }

            if(node != null)
            {
                if(!tag.HasChild || node.Children == null || node.Children.Length == 0)
                {
                    return node;
                }

                if(node.HasChildNodes && tag.HasChild)
                {
                    List<IElement> descendantNodes     = node.Children.Where(child => child.LocalName == tag.Child.ToString()).ToList();
                    IElement       foundDescendantNode = null;

                    if(descendantNodes.Count == 0)
                    {
                        return node.Children.FirstOrDefault();
                    }

                    if(tag.Child.HasIndex && descendantNodes.Count >= tag.Child.Index && tag.Child.Index >= 1)
                    {
                        foundDescendantNode = descendantNodes[tag.Child.Index - 1].GetElementByTags(tag.Child);
                    }
                    else
                    {
                        foundDescendantNode = descendantNodes[0].GetElementByTags(tag.Child);
                    }

                    if(foundDescendantNode != null)
                    {
                        return foundDescendantNode;
                    }
                }
            }

            return null;
        }

        public static List<IElement> GetElementsByTags(this IElement node,
                                                       HtmlTag       tag)
        {
            if(node.LocalName != tag.ToString())
            {
                throw new Exception();
            }

            if(node != null)
            {
                if(!tag.HasChild)
                {
                    return node.Children.Where(child => child.LocalName == tag.Child.ToString()).ToList();
                }

                if(node.HasChildNodes && tag.HasChild)
                {
                    List<IElement> descendantNodes     = node.Children.Where(child => child.LocalName == tag.Child.ToString()).ToList();
                    List<IElement> foundDescendantNode = null;

                    if(descendantNodes.Count == 0)
                    {
                        return new List<IElement>()
                        {
                            node.Children.FirstOrDefault()
                        };
                    }

                    if(!tag.Child.HasChild)
                    {
                        return descendantNodes;
                    }

                    if(tag.Child.HasIndex && descendantNodes.Count >= tag.Child.Index && tag.Child.Index >= 1)
                    {
                        foundDescendantNode = descendantNodes[tag.Child.Index - 1].GetElementsByTags(tag.Child);
                    }
                    else
                    {
                        foundDescendantNode = descendantNodes[0].GetElementsByTags(tag.Child);
                    }

                    if(foundDescendantNode != null)
                    {
                        return foundDescendantNode;
                    }
                }
            }

            return null;
        }

        //public static IElement GetElementWithXPath(this IElement node,
        //                                           string        xPath)
        //{
        //    if(node != null)
        //    {
        //        if(node.XPath == xPath)
        //        {
        //            return node;
        //        }
        //
        //        if(node.HasChildNodes)
        //        {
        //            INodeList descendantNodes     = node.ChildNodes;//.Where(child => child.Name != HtmlTags.text);
        //            IElement              foundDescendantNode = null;
        //
        //            Parallel.ForEach(descendantNodes,
        //                             (descendantNode,
        //                              loopState) =>
        //                             {
        //                                 IElement descendantFoundNode = ((IElement)descendantNode).GetElementWithXPath(xPath);
        //
        //                                 if(descendantFoundNode != null)
        //                                 {
        //                                     foundDescendantNode = descendantFoundNode;
        //                                     loopState.Stop();
        //                                 }
        //                             });
        //
        //            if(foundDescendantNode != null)
        //            {
        //                return foundDescendantNode;
        //            }
        //        }
        //    }
        //
        //    return null;
        //}
        //
        //public static IElement GetElementWithInnerText(this IElement node,
        //                                               string        innerText)
        //{
        //    if(node != null)
        //    {
        //        if(node.InnerHtml == innerText)
        //        {
        //            return node;
        //        }
        //
        //        if(node.HasChildNodes)
        //        {
        //            INodeList descendantNodes     = node.ChildNodes;//.Where(child => child.Name != HtmlTags.text);
        //            IElement              foundDescendantNode = null;
        //
        //            Parallel.ForEach(descendantNodes,
        //                             (descendantNode,
        //                              loopState) =>
        //                             {
        //                                 IElement descendantFoundNode = ((IElement)descendantNode).GetElementWithInnerText(innerText);
        //
        //                                 if(descendantFoundNode != null)
        //                                 {
        //                                     foundDescendantNode = descendantFoundNode;
        //                                     loopState.Stop();
        //                                 }
        //                             });
        //
        //            if(foundDescendantNode != null)
        //            {
        //                return foundDescendantNode;
        //            }
        //        }
        //    }
        //
        //    return null;
        //}

        //public static IEnumerable<IElement> GetElementsWithClass(this IElement node,
        //                                                         string        className)
        //{
        //    if(node == null)
        //    {
        //        return null;
        //    }
        //
        //    string         attributeValue;
        //    List<IElement> foundNodes = new List<IElement>();
        //
        //    if(node.Attributes.Length > 0)
        //    {
        //        //Debug.Write(string.Join(", ", node.Attributes.Select(attr => attr.Name + " " + attr.Value + " ")));
        //        attributeValue = node.GetAttribute(HtmlTags.@class,
        //                                           string.Empty);
        //
        //        if(!string.IsNullOrEmpty(attributeValue) && attributeValue.Contains(className))
        //        {
        //            foundNodes.Add(node);
        //        }
        //    }
        //
        //    if(node.HasChildNodes)
        //    {
        //        INodeList descendantNodes = node.ChildNodes;//.Where(child => child.Name != HtmlTags.text);
        //
        //        //foreach(IElement descendantNode in descendantNodes)
        //        //{
        //        //    IEnumerable<IElement> descendantFoundNodes = GetElementsWithClass(descendantNode, className);
        //        //    foundNodes.AddRange(descendantFoundNodes);
        //        //}
        //        Parallel.ForEach(descendantNodes,
        //                         (descendantNode,
        //                          loopState) =>
        //                         {
        //                             IEnumerable<IElement> descendantFoundNodes = ((IElement)descendantNode).GetElementsWithClass(className);
        //
        //                             foundNodes.AddRange(descendantFoundNodes);
        //                         });
        //    }
        //
        //    return foundNodes;
        //}

        #endregion
    }
}