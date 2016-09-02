function plot_octspect(sig,srate,octres,range,points,order)% function plot_octspect(sig,srate,octres,range,points,order)if (nargin<6 | isempty(order)) order = 2; endif (nargin<3 | isempty(octres)) octres = 0.33; endif (nargin<5 | isempty(points)) points = fix(4*10/octres); endif (nargin<4 | isempty(range)) range = [20,0.5*0.99*srate]; endfn = logspace(log10(range(1)),log10(range(2)),points);cf = 2^(octres/2);for k=1:points	f = fn(k); freqs(k) = f;	fl = f/cf; fh = f*cf;	fdif = fh-fl; fc = 0.5*srate/fdif;	if (fh>=srate/2) fh = 0.49*srate; end	ord = order; if f<50 ord = 2; end	[b,a] = butter(ord,2*[fl,fh]/srate);	sign = filter(b,a,sig);	impf = filter(b,a,imp(length(sig)));	level(k) = db(norm(sign)/norm(impf));endsemilogx(freqs,level);