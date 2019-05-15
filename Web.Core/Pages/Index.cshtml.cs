namespace Web.Core.Pages
{
    using System;
    using Microsoft.AspNetCore.Mvc.RazorPages;

    public class Index : PageModel
    {
        public void OnGet(string a)
        {
            if (string.IsNullOrEmpty(a) == false)
                throw new Exception("Some error happened");
        }
    }
}
