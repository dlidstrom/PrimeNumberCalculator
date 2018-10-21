namespace PrimeNumberCalculator.ViewComponents
{
    using System.Threading.Tasks;
    using Microsoft.AspNetCore.Mvc;
    using PrimeNumberCalculator.Model;

    [ViewComponent]
    public class SideBarViewComponent : ViewComponent
    {
        public async Task<IViewComponentResult> InvokeAsync()
        {
            var urls = await Task.FromResult(new []
            {
                new Link { Url = "https://snittlistan.se", Text = "Snittlistan" }
            });

            return View(urls);
        }
    }
}
