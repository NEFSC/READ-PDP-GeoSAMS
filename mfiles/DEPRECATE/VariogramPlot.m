function VariogramPlot(title)

outputDir = 'Results/';

flnm = [outputDir, title, 'DIntV.txt'];
x = load(flnm);
flnm = [outputDir, title, 'GammaIntV.txt'];
y1 = load(flnm);
flnm = [outputDir, title, 'SIntV.txt'];
y2=load(flnm);

len = size(x,1);

figure('Name', [title, int2str(len)]);
plot(x,y1, x, y2)
xlabel('DIntV')
legend('GammaIntV','SIntV')
grid

saveas(gcf,[outputDir, title, int2str(len), '.pdf'])