%%% BC RESPONSE ANALYSIS
bc_init;
UR_resp = [UR;zeros(8820-1,1)];
IE_resp = [IE;zeros(8820-1,1)];
for I_LOOP = 2:8820
  bc_step;
  UR_resp(I_LOOP) = UR;
  IE_resp(I_LOOP) = IE;
end

figure(10); clf; subplot(2,1,1)
plot_sig(UR_resp,SRATE); grid on; 
xlabel('Time [s]');
ylabel('V_R [V]');
title('Load resistor voltage');
subplot(2,1,2);
plot_sig(1000*IE_resp,SRATE); grid on; 
xlabel('Time [s]');
ylabel('I_D [mA]');
axis([0 0.2 -5 35]);
title('Diode current');