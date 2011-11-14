// Create a closure that acts as a continuation for a sequence of jQuery get
// calls
function phaseContinuation(phase_list, options, finalizer) {
  var phase_order = {};
  for (i=0; i<phase_list.length; i++) { phase_order[phase_list[i]] = i; }

  var count = phase_list.length;
  var phases = [ ];
  return function(job_name, phase_data) {
    console.log("Adding phase "+job_name);
    count--;
    json_ready = phase_data
      .replace(/[\n\t\r]/g,' ')
    phase_spec = jQuery.parseJSON(json_ready);
    phases[phase_order[job_name]] = {name:job_name, spec:phase_spec};
    if (count == 0) { finalizer(phases, options); }
  }
}

// This is the finalizer function of the continuation
function runPhases(phases, options) {
  bucket = options['bucket']
  context = options['context']
  var mapper = new RiakMapper(Rekon.client, bucket);
  // p is an object {name:'job_name.map', spec:<json>}
  phases.map(function(p) { addJob(mapper,p) });
  mapper.run(null, function(status, list, xmlrequest) {
    if (! status) {
      context.render('bucket-err.html.template')
        .replace('#keys tbody')
        .wait();
      return;
    }

    keyRows = list.map(function(obj) {
      s = "";
      // TODO Make formatting a custom function
      switch (typeof obj)
      {
        case "object":
          for (k in obj) { s += k + " : " + obj[k] + "\n"; };
          break;
        default: s = obj; break;
      }
      return {value:s};
    });
    context.renderEach('key-mr.html.template', keyRows)
      .replace('#keys tbody')
      .then(function(){ searchable('#bucket table tbody tr'); })
  });
}

function addJob(mapper, p) {
  if (/\.map$/i.test(p.name)) { mapper.map(p.spec); }
  else if (/\.red$/i.test(p.name)) { mapper.reduce(p.spec); }
  else { console.log("Skipping unknown file extension for "+p.name); }
}

rekonApp = Sammy(function() {

  $container = $(this.$element);

  header = function(header, url) {
    $container.find('h1').html(header + " &ndash; <em> " + url + "</em>");
  };

  breadcrumb = function(crumb) {
    $('<li>').append(crumb).appendTo('ul#footer-navi');
  };

  searchable = function(selector) {
    $('#row_search').quicksearch(selector, {selector: 'th'});
  };

  // Render phases that are loaded in the bucket
  renderPhase = function(selector, filter, callback) {
    if (!filter) { filter = find_maps; }
    var bucket = new RiakBucket('rekon.jobs', Rekon.client);
    bucket.keys(function(keys) {
      if (keys.length < 1) return;

      keys = keys.filter(filter);
      phases = keys
        .map(function(x) { return '<option value="'+x+'">'+x+'</option>'; })
        .join();
      $(selector).each(function(idx,el) { $(el).append(phases); });

      if (callback) { callback(); }
    });
  };
  
  
  this.use('Template');
  this.use('NestedParams');

  this.before(function(){
    $('#main').empty();
    $('#content h1').html('');
    $('#footer-navi li:not(.perm)').remove();
  });

  this.get('#/buckets', function(context){
    header('Buckets', Rekon.baseUrl());

    context.render('buckets.html.template').appendTo('#main');
    
    Rekon.client.buckets(function(buckets) {
      bucketRows = buckets.map(function(bucket){ return {bucket: bucket};});
      context.renderEach('bucket-row.html.template', bucketRows).replace('#buckets tbody').then(
        function(){ searchable('#buckets table tbody tr'); }
      );
    });
  });

  this.get('#/buckets/:bucket', function(context){
    var name   = this.params['bucket'];
    var bucket = new RiakBucket(name, Rekon.client);
    
    header('Bucket', Rekon.riakUrl(name));
    breadcrumb($('<a>').attr('href', '#/bucket-props/' + name).text('Props'));
    breadcrumb($('<a>').attr('href', Rekon.riakUrl(name))
      .attr('target', '_blank').text('Riak').addClass('action'));

    context.render('bucket-mr.html.template', {bucket:name})
      .appendTo('#main')
      .wait();
    renderPhase('#mr_jobs select[multiple]', find_mrs, function() {
      $("#mr_jobs select[multiple]")
        .asmSelect({sortable: true, animate: true});
    });
    context.render('bucket.html.template', {bucket:name})
      .appendTo('#main')
      .wait();

    bucket.keys(function(keys) {
      if (keys.length > 0) {
        keyRows = keys.map(
          function(key) { return {bucket:name, key:key}; });
        context.renderEach('key-row.html.template', keyRows)
          .replace('#keys tbody')
          .then(function(){ searchable('#bucket table tbody tr'); })
          .then(function(){ renderPhase('#keys select#phase'); });
      } else {
        context.render('bucket-empty.html.template').replace('#keys tbody');
      }
    });
  });

  this.get('#/bucket-props/:bucket', function(context) {
    var name   = this.params['bucket'];
    var bucket = new RiakBucket(name, Rekon.client);

    header('Bucket Properties', Rekon.riakUrl(name));
    breadcrumb($('<a>').attr('href', '#/buckets/' + name).text('Keys'));
    breadcrumb($('<a>').attr('href', Rekon.riakUrl(name)).attr('target', '_blank').text('Riak').addClass('action'));

    bucket.getProps(function(props) {
      var pre_commit, post_commit;
      pre_commit  = props.precommit.join(",");
      post_commit = props.postcommit.join(",");
      if(pre_commit === "") {pre_commit = "None";}
      if(post_commit === "") {post_commit = "None";}
      context.render('bucket-hooks.html.template', {pre_commit: pre_commit, post_commit: post_commit},
        function(){
          context.render('bucket-props.html.template', {props: props}).appendTo('#main').then(function(){
            var $selects, $select, i;
            $selects = $('select[data-select-value]');
            /* select the nvalue */
            $('select#n_val').val($('select#n_val').attr('data-select-value'));
            /* bind the limit based off of the n_val */
            Rekon.capControlsSelector();
            /* reselect cap control vals based off of nval */
            for(i=0; i<$selects.length;i++) { 
              $select = $($selects[i]);
              $select.val($select.attr('data-select-value'));
            }
            $('select#n_val').change(Rekon.capControlsSelector);
          });
        }
      ).appendTo('#main');
    });
  });

  this.get('#/buckets/:bucket/:key', function(context) {
    var name   = this.params['bucket'];
    var key    = this.params['key'];
    var bucket = new RiakBucket(name, Rekon.client);

    header('Key', Rekon.riakUrl(name + '/' + key));
    breadcrumb($('<a>').attr('href', '#/buckets/' + name).text('Keys'));
    breadcrumb($('<a>').attr('href', '#/buckets/' + name + '/' + key + '/edit').text('Edit').addClass('action'));
    breadcrumb($('<a>').attr('href', Rekon.riakUrl(name + '/' + key)).attr('target', '_blank').
      text('Riak').addClass('action'));

    context.render('key.html.template').appendTo('#main');

    bucket.get(key, function(status, object) {
      context.render('key-content-type.html.template', {object: object}, function(){
        context.render('key-meta.html.template', {object: object}).appendTo('#key tbody');
      }).appendTo('#key tbody');

      switch(object.contentType) {
      case 'image/png':
      case 'image/jpeg':
      case 'image/jpg':
      case 'image/gif':
        context.render('value-image.html.template', {bucket: name, key: key}).appendTo('#value');
        return;
      case 'application/json':
        value = JSON.stringify(object.body, null, 4);
        break;
      default:
        value = object.body;
        break;
      }
      context.render('value-pre.html.template', {value: value}).appendTo('#value');
    });
  });


  // Don't use edit since it will try to save JSON. We want to preserve data
  this.post('#/buckets/:bucket/:key0/move', function(context){ 
    var app    = this;
    var name   = this.params['bucket'];
    var key    = this.params['key0'];
    var target = this.params['key1'];
    var bucket = new RiakBucket(name, Rekon.client);

    bucket.get(key, function(status, object) {
      object.key = target;

      object.store(function(status, rObject) {
        switch(status) {
        case 'siblings':
          alert("Oh noes! Siblings have been born and Rekon doesn't handle that yet.");
          break;
        case 'failure':
          alert("There was an error saving to Riak.");
          break;
        case 'ok':
        default:
          // Only delete the old object if the new one was added successfully!
          $.ajax({
            type: 'DELETE',
            url: Rekon.riakUrl(name + '/' + key)
          });
          $("#simplemodal-overlay").remove();
          $("#simplemodal-container").remove();
          app.redirect('#/buckets/' + name + '/' + target);
          break;
        }
      });
    });
  });


  this.get('#/buckets/:bucket/:key/edit', function(context) {
    var name   = this.params['bucket'];
    var key    = this.params['key'];
    var bucket = new RiakBucket(name, Rekon.client);
    var app    = this;

    header('Edit Key', Rekon.riakUrl(name + '/' + key));
    breadcrumb($('<a>').attr('href', '#/buckets/' + name).text('Keys'));
    breadcrumb($('<a>').attr('href', '#/buckets/' + name + '/' + key).text('View').addClass('action'));
    breadcrumb($('<a>').attr('href', Rekon.riakUrl(name + '/' + key)).attr('target', '_blank').
      text('Riak').addClass('action'));

    context.render('edit-key.html.template', {bucket: name, key: key}).appendTo('#main');

    bucket.get(key, function(status, object) {
      switch(object.contentType) {
      case 'image/png':
      case 'image/jpeg':
      case 'image/jpg':
      case 'image/gif':
        alert('Image editing is not supported currently.');
        app.redirect('#/buckets/' + name + '/' + key);
        return;
      case 'application/json':
        value = JSON.stringify(object.body, null, 4);
        break;
      default:
        value = object.body;
        break;
      }
      context.render('edit-key-content-type.html.template', {object: object}, function(html){
        context.render('key-meta.html.template', {object: object}).appendTo('#edit-key tbody');
      }).appendTo('#edit-key tbody').then(function(html){
        $select = $('select[name=content-type]');
        $select.val(object.contentType);
      });
      context.render('edit-value.html.template', {value: value}).appendTo('#edit-value');
    });
  });

  this.get('#/stats', function(context){
    header('Node Stats', document.location.origin + "/stats");

    $.getJSON('/stats', function(data) {
      context.render('stats.html.template', {stats:data}).appendTo('#main').then(
        function(){ searchable('#stats tbody tr'); }
      );
    });
  });

  this.post('#/buckets', function(context) {
    var name = this.params['bucket'];
    this.redirect('#/buckets/' + name);
  });

  this.post('#/buckets/:bucket', function(context){
    var app    = this;
    var name   = this.params['bucket'];
    var key    = this.params['key'] === '' ? undefined : this.params['key'];
    var object = new RiakObject(name, key, Rekon.client, '{}', 'application/json');
    object.store(function(status, rObject){
      switch(status) {
      case 'siblings':
        alert("Oh noes! Siblings have been born and Rekon doesn't handle that yet.");
        break;
      case 'failure':
        alert("There was an error creating a new Riak object.");
        break;
      case 'ok':
      default:
        console.log(rObject);
        app.redirect('#/buckets/' + name + '/' + rObject.key);
        break;
      }
    });
  });

  this.post('#/bucket-props/:bucket', function(context) {
    var app      = this;
    var name     = this.params['bucket'];
    var bucket   = new RiakBucket(name, Rekon.client);
    var props    = Rekon.typecastBucketProps(this.params['props']);

    bucket.props = props;
    bucket.store(function(){
      app.redirect("#/bucket-props/" + name);
    });
  });

  this.post('#/buckets/:bucket/:key', function(context){ 
    var app    = this;
    var name   = this.params['bucket'];
    var key    = this.params['key'];
    var bucket = new RiakBucket(name, Rekon.client);

    bucket.get(key, function(status, object) {
      object.contentType = app.params['content-type'];
      object.body        = app.params['value'];

      if (object.contentType == 'application/json') {
        object.body = JSON.parse(object.body);
      }

      object.store(function(status, rObject) {
        switch(status) {
        case 'siblings':
          alert("Oh noes! Siblings have been born and Rekon doesn't handle that yet.");
          break;
        case 'failure':
          alert("There was an error saving to Riak.");
          break;
        case 'ok':
        default:
          app.redirect('#/buckets/' + name + '/' + key);
          break;
        }
      });
    });
  });

  this.get('#/stats', function(context){
    header('Node Stats', document.location.origin + "/stats");

    $.getJSON('/stats', function(data) {
      context.render('stats.html.template', {stats:data}).appendTo('#main').then(
        function(){ searchable('#stats tbody tr'); }
      );
    });
  });

  this.get('#/luwak', function(context){
    luwak = new Luwak(Rekon.client);

    header('Luwak', document.location.origin + "/luwak");
    context.render('luwak.html.template').appendTo('#main').then(function(){

      luwak.files(function(files) {
        if (files === null) {
          console.log('not working');
          $('#files .pending td').html(
          '<p><b>Luwak is not enabled.</b> Please add <code>{luwak, [{enabled, true}]}</code> to your app.config.</p>');
        }
        else if (files.length > 0) {
          fileRows = files.map(function(file){ return {file:file};});
          context.renderEach('luwak-row.html.template', fileRows)
            .replace('#files tbody')
            .then(function() { searchable('#luwak tbody'); }
          );
        } else{
          $('#files .pending td').html('<p>You have not added any files to luwak.</p>');
        }
      });
    });
  });

  /*************************** MAP REDUCE JOBS ******************************/
  this.get('#/mapred', function(context) {
  });

  /** Run a map on a single key **/
  this.post('#/mapred/:bucket/:key', function(context) {
    var name   = this.params['bucket'];
    var key    = this.params['key'];

    header('Key', Rekon.riakUrl(name + '/' + key));
    breadcrumb($('<a>').attr('href', '#/buckets/' + name).text('Keys'));
    breadcrumb($('<a>').attr('href', '#/buckets/' + name + '/' + key + '/edit').text('Edit').addClass('action'));
    breadcrumb($('<a>').attr('href', Rekon.riakUrl(name + '/' + key)).attr('target', '_blank').
      text('Riak').addClass('action'));

    context.render('key.html.template').appendTo('#main');

    phase_url = Rekon.riakUrl('rekon.jobs/'+this.params['phase']);
    var phase  = jQuery.get(phase_url, function(data) {
      phase_data = jQuery.parseJSON(data);
      var mapper = new RiakMapper(Rekon.client, name, key);
      mapper.map(phase_data);
      mapper.run(null, function(status, list, xmlrequest) {
        if (! status) {
          // TODO: Add error message.
          return;
        }

        object = list[0];
        context.render('key-content-type.html.template', {object: object}, function(){
          context.render('key-meta.html.template', {object: object}).appendTo('#key tbody');
        }).appendTo('#key tbody');

        switch(object.contentType) {
        case 'image/png':
        case 'image/jpeg':
        case 'image/jpg':
        case 'image/gif':
          context.render('value-image.html.template', {bucket: name, key: key}).appendTo('#value');
          return;
        case 'application/json':
          value = JSON.stringify(object, null, 4);
          break;
        default:
          value = object;
          break;
        }
        context.render('value-pre.html.template', {value: value}).appendTo('#value');
      });
    });
  });

  /** Run a map/reduce on a bucket **/
  this.post('#/mapred/:bucket', function(context) {
    var name   = this.params['bucket'];

    header('Key', Rekon.riakUrl(name));
    breadcrumb($('<a>').attr('href', '#/buckets/' + name).text('Keys'));
    breadcrumb($('<a>').attr('href', Rekon.riakUrl(name))
      .attr('target', '_blank')
      .text('Riak').addClass('action'));

    context.render('bucket-mr.html.template', {bucket:name})
      .appendTo('#main')
      .wait();
    renderPhase('#mr_jobs select[multiple]', find_mrs, function() {
      $("#mr_jobs select[multiple]")
        .asmSelect({sortable: true, animate: true});
    });
    context.render('bucket.html.template', {bucket:name})
      .appendTo('#main')
      .wait();

    raw_phases = jQuery.parseJSON(this.params['phases']);
    options = {bucket:name,context:context};
    continuation = phaseContinuation(raw_phases, options, runPhases);
    raw_phases.map(function(p) {
      phase_url = Rekon.riakUrl('rekon.jobs/'+p);
      jQuery.get(phase_url, function(data) { continuation(p, data) });
    });
  });

});



Rekon = {
  client : new RiakClient(),

  locationUrl : function() {
    return document.location.protocol + '//' + document.location.host;
  },

  baseUrl : function() {
    return this.locationUrl() + this.client.baseUrl;
  },

  luwakUrl : function() {
    return this.locationUrl() + this.client.luwakUrl;
  },

  riakUrl : function(append) {
    if (append === undefined) {
      append = "";
    }
    return this.baseUrl() + append;
  },

  typecastBucketProps : function(props) {
    keys = ['w', 'r', 'dw', 'rw', 'n_val', 'young_vclock', 'old_vclock', 'small_vclock', 'big_vclock'];
    for(var i=0; i<keys.length; i++) {
      key = keys[i];
      val = parseInt(props[key], 10);
      if (val) {
        props[key] = parseInt(props[key], 10);
      }
    }
    props.allow_mult      = !!props.allow_mult;
    props.last_write_wins = !!props.last_write_wins;

    return props;
  },

  capControlsSelector : function() {
    var nVal = parseInt($('select#n_val').val(), 10);
    $('.cap-control').each(function(i, select) {
      var $select = $(select);
      var value   = parseInt($select.val(), 10);
      var endVal  = parseInt($select.find('option:last').val(), 10);
      if (isNaN(endVal)) { endVal = 0; }

      /* figure out if we need to append or trim */
      if (endVal > nVal) {
        $select.find('option').each(function(j, option) {
          var $option = $(option);
          if (parseInt($option.val(), 10) > nVal) {
            $option.remove();
          }
        });
        if (value) {
          $select.val($select.find('option:last').val());
        }
      } 
      else if (endVal < nVal) {
        while(endVal < nVal) {
          endVal++;
          $('<option>').val(endVal).html(endVal).appendTo($select);
        } 
      }
    });
  }

};

// Set the form field to contain all selected elements
$('#mr_jobs button').live('click', function(e){
  var link = this;
  //e.preventDefault();
  var jobs = [ ];
  $('#mr_jobs :selected')
    .each(function(i, selected) { if (i>0) {jobs[i-1] = $(selected).val();} });
  $('#mr_phases').val(JSON.stringify(jobs));
});

$('#keys a.move').live('click', function(e){
  var link = this;
  e.preventDefault();
  $(e.target).next("div").modal();
});

$('#keys a.delete').live('click', function(e){
  var link = this;
  e.preventDefault();
  if(!confirm("Are you sure you want to delete:\n" + $(link).attr('href'))) { return; }

  $.ajax({
    type: 'DELETE',
    url: $(link).attr('href')
  }).success(function(){
    $(link).closest('tr').remove();
  }).error(function(){
    alert('There was an error deleting this object from Riak.');
  });
});

var filterInteger = function(){
  var value = parseInt($(this).val(), 10);
  if (isNaN(value)) {
    value = 1;
  }
  $(this).val(value);
};

$("input[data-filter=integer]").live('blur', filterInteger);


// Just find map files (JS ending in .map)
var find_maps = function(key) {
  return /\.map$/.test(key);
}

// Find both map and reduce files (JS ending in .map or .red)
var find_mrs = function(key) {
  return /\.(map|red)$/.test(key);
}

/*
* Bootstrap the application
*/
jQuery(function($) {
  rekonApp.run('#/buckets');
});
