---
title: "Contact2"
type: home
weight: 100
---


<form method="post" action="https://formspree.io/f/moqrjlze">
  <div class="form-group row">
    <label for="name" class="col-4 col-form-label">Name</label>
    <div class="col-8">
      <div class="input-group">
        <div class="input-group-addon">
          <i class="fa fa-user"></i>
        </div>
        <input id="name" name="name" placeholder="Please enter your name" type="text" required="required" class="form-control">
      </div>
    </div>
  </div>
  <div class="form-group row">
    <label for="email" class="col-4 col-form-label">E-mail address</label>
    <div class="col-8">
      <div class="input-group">
        <div class="input-group-addon">
          <i class="fa fa-envelope"></i>
        </div>
        <input id="email" name="email" placeholder="Your e-mail address" type="text" required="required" class="form-control">
      </div>
    </div>
  </div>
  <div class="form-group row">
    <label for="message" class="col-4 col-form-label">Message</label>
    <div class="col-8">
      <textarea id="message" name="message" cols="40" rows="10" required="required" class="form-control"></textarea>
    </div>
  </div>
  <html>
  <head>
    <title>reCAPTCHA demo: Simple page</title>
    <script src="https://www.google.com/recaptcha/api.js" async defer></script>
  </head>
  <body>
    <form action="https://formspree.io/f/moqrjlze" method="POST">
      <div class="g-recaptcha" data-sitekey="6LfwoyAeAAAAAFO8fuhGuGXSGzlMEHUd-vdd9LQr"></div> <!-- replace with your recaptcha SITE key not secret key -->
      <br/>
      <input type="submit" value="Submit">
    </form>
  </body>
</html>

  
</form>



