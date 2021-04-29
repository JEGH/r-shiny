function Init(){
            var input_pass=document.getElementById('passwd');
            var input_user=document.getElementById('userName');
            if(input_pass==null || input_user==null){setTimeout(Init,100);}
            else{  
              // Execute a function when the user releases a key on the keyboard
              input_pass.addEventListener('keyup', function(event) {
                  // Cancel the default action, if needed
                  event.preventDefault();
                  // Number 13 is the 'Enter' key on the keyboard
                  if (event.keyCode === 13) {
                    // Trigger the button element with a click
                    var login=document.getElementById('Login');
                    login.click();
                    login.innerHTML='Entering ...';
                    setTimeout(function(){login.innerHTML='Log in';},1000);
                  }
              });
              input_user.addEventListener('keyup', function(event) {
                  // Cancel the default action, if needed
                  event.preventDefault();
                  // Number 13 is the 'Enter' key on the keyboard
                  if (event.keyCode === 13) {
                    // Trigger the button element with a click
                    var login=document.getElementById('Login');
                    login.click();
                    login.innerHTML='Entering ...';
                    setTimeout(function(){login.innerHTML='Log in';},1000);
                  }
              });
            }
        }
        Init();