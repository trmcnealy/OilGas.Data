using System;
using System.Diagnostics;

// ReSharper disable once CheckNamespace
namespace System.Runtime.Versioning
{
    [AttributeUsage(AttributeTargets.Class | AttributeTargets.Struct | AttributeTargets.Method | AttributeTargets.Constructor,
                    AllowMultiple = false,
                    Inherited     = false)]
    public sealed class NonVersionableAttribute : Attribute
    {
        public NonVersionableAttribute()
        {
        }
    }
}