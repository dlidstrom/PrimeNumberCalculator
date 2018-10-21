namespace PrimeNumberCalculator.Api
{
    using Microsoft.AspNetCore.Mvc;

    [Route("api/users")]
    public class UsersApiController : Controller
    {
        // curl -k https://localhost:5001/api/users
        [HttpGet]
        public IActionResult Get()
        {
            var users = new[]
            {
                new UserDto
                {
                    Id = 1,
                    Name = "Daniel"
                },
                new UserDto
                {
                    Id = 2,
                    Name = "Kjell"
                }
            };

            return Ok(users);
        }

        // curl -v -k -X POST -H "Content-Type: application/json" https://localhost:5001/api/users -d '{"Id": 1, "Name": "Daniel"}'
        [HttpPost]
        public IActionResult Post([FromBody] UserDto user)
        {
            return CreatedAtAction("Get", user);
        }

        public class UserDto
        {
            public int Id { get; set; }

            public string Name { get; set; }
        }
    }
}
