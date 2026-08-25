import { useDataStore } from '@/stores/dataStore'
import { useAppStore } from '@/stores/appStore'
import { formatDuration } from '@/lib/utils'
import { Link } from 'react-router-dom'

interface StatCardProps {
  title: string
  value: string | number
  subtitle?: string
  icon: React.ReactNode
}

function StatCard({ title, value, subtitle, icon }: StatCardProps) {
  return (
    <div className="rounded-lg border bg-card p-6 shadow-sm">
      <div className="flex items-center justify-between">
        <div>
          <p className="text-sm font-medium text-muted-foreground">{title}</p>
          <p className="mt-2 text-3xl font-bold">{value}</p>
          {subtitle && <p className="mt-1 text-xs text-muted-foreground">{subtitle}</p>}
        </div>
        <div className="text-muted-foreground">{icon}</div>
      </div>
    </div>
  )
}

interface RecentDatasetProps {
  id: string
  name: string
  nChannels: number
  duration: number
  samplingRate: number
}

function RecentDatasetCard({ id, name, nChannels, duration, samplingRate }: RecentDatasetProps) {
  return (
    <Link to={`/data/${id}`} className="block rounded-lg border bg-card p-4 transition-colors hover:bg-accent">
      <h3 className="font-medium truncate">{name}</h3>
      <div className="mt-2 flex gap-4 text-sm text-muted-foreground">
        <span>{nChannels} ch</span>
        <span>{formatDuration(duration)}</span>
        <span>{samplingRate} Hz</span>
      </div>
    </Link>
  )
}

export function Dashboard() {
  const { datasets } = useDataStore()
  const { isConnected, notifications } = useAppStore()

  const datasetList = Object.values(datasets)
  const totalChannels = datasetList.reduce((sum, d) => sum + d.nChannels, 0)
  const totalDuration = datasetList.reduce((sum, d) => sum + d.duration, 0)
  const recentNotifications = notifications.slice(-5).reverse()

  return (
    <div className="space-y-8">
      <div>
        <h1 className="text-3xl font-bold">Dashboard</h1>
        <p className="mt-2 text-muted-foreground">PhysioExperiment ワークスペースの概要</p>
      </div>

      <div className="grid gap-4 md:grid-cols-2 lg:grid-cols-4">
        <StatCard title="データセット数" value={datasetList.length} icon={
          <svg className="h-8 w-8" fill="none" viewBox="0 0 24 24" stroke="currentColor">
            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={1.5} d="M4 7v10c0 2.21 3.582 4 8 4s8-1.79 8-4V7M4 7c0 2.21 3.582 4 8 4s8-1.79 8-4M4 7c0-2.21 3.582-4 8-4s8 1.79 8 4" />
          </svg>
        } />
        <StatCard title="総チャンネル数" value={totalChannels} icon={
          <svg className="h-8 w-8" fill="none" viewBox="0 0 24 24" stroke="currentColor">
            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={1.5} d="M13 10V3L4 14h7v7l9-11h-7z" />
          </svg>
        } />
        <StatCard title="総記録時間" value={formatDuration(totalDuration)} icon={
          <svg className="h-8 w-8" fill="none" viewBox="0 0 24 24" stroke="currentColor">
            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={1.5} d="M12 8v4l3 3m6-3a9 9 0 11-18 0 9 9 0 0118 0z" />
          </svg>
        } />
        <StatCard title="接続状態" value={isConnected ? 'オンライン' : 'オフライン'} subtitle={isConnected ? 'R バックエンド接続中' : '未接続'} icon={
          <div className={`h-4 w-4 rounded-full ${isConnected ? 'bg-green-500' : 'bg-red-500'}`} />
        } />
      </div>

      <div className="grid gap-6 lg:grid-cols-2">
        <div className="rounded-lg border bg-card">
          <div className="border-b px-6 py-4"><h2 className="font-semibold">最近のデータセット</h2></div>
          <div className="p-4">
            {datasetList.length === 0 ? (
              <div className="py-8 text-center text-muted-foreground">
                <svg className="mx-auto h-12 w-12" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                  <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={1.5} d="M9 13h6m-3-3v6m5 5H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z" />
                </svg>
                <p className="mt-4">データセットがありません</p>
                <Link to="/data" className="mt-2 inline-block text-sm text-primary hover:underline">データをインポート →</Link>
              </div>
            ) : (
              <div className="space-y-2">
                {datasetList.slice(0, 5).map((dataset) => (
                  <RecentDatasetCard key={dataset.id} id={dataset.id} name={dataset.name} nChannels={dataset.nChannels} duration={dataset.duration} samplingRate={dataset.samplingRate} />
                ))}
              </div>
            )}
          </div>
        </div>

        <div className="rounded-lg border bg-card">
          <div className="border-b px-6 py-4"><h2 className="font-semibold">クイックアクション</h2></div>
          <div className="grid gap-3 p-4">
            <Link to="/data" className="flex items-center gap-3 rounded-lg border p-4 transition-colors hover:bg-accent">
              <svg className="h-6 w-6 text-primary" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M4 16v1a3 3 0 003 3h10a3 3 0 003-3v-1m-4-8l-4-4m0 0L8 8m4-4v12" />
              </svg>
              <div><p className="font-medium">データインポート</p><p className="text-sm text-muted-foreground">EDF, BDF, BrainVision ファイルを読み込み</p></div>
            </Link>
            <Link to="/viewer" className="flex items-center gap-3 rounded-lg border p-4 transition-colors hover:bg-accent">
              <svg className="h-6 w-6 text-primary" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M9 19v-6a2 2 0 00-2-2H5a2 2 0 00-2 2v6a2 2 0 002 2h2a2 2 0 002-2zm0 0V9a2 2 0 012-2h2a2 2 0 012 2v10m-6 0a2 2 0 002 2h2a2 2 0 002-2m0 0V5a2 2 0 012-2h2a2 2 0 012 2v14a2 2 0 01-2 2h-2a2 2 0 01-2-2z" />
              </svg>
              <div><p className="font-medium">信号ビューア</p><p className="text-sm text-muted-foreground">時系列データの可視化と探索</p></div>
            </Link>
            <Link to="/analysis" className="flex items-center gap-3 rounded-lg border p-4 transition-colors hover:bg-accent">
              <svg className="h-6 w-6 text-primary" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M9 7h6m0 10v-3m-3 3h.01M9 17h.01M9 14h.01M12 14h.01M15 11h.01M12 11h.01M9 11h.01M7 21h10a2 2 0 002-2V5a2 2 0 00-2-2H7a2 2 0 00-2 2v14a2 2 0 002 2z" />
              </svg>
              <div><p className="font-medium">解析ワークスペース</p><p className="text-sm text-muted-foreground">フィルタ、エポッキング、統計解析</p></div>
            </Link>
            <Link to="/workflow" className="flex items-center gap-3 rounded-lg border p-4 transition-colors hover:bg-accent">
              <svg className="h-6 w-6 text-primary" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M4 5a1 1 0 011-1h14a1 1 0 011 1v2a1 1 0 01-1 1H5a1 1 0 01-1-1V5zM4 13a1 1 0 011-1h6a1 1 0 011 1v6a1 1 0 01-1 1H5a1 1 0 01-1-1v-6zM16 13a1 1 0 011-1h2a1 1 0 011 1v6a1 1 0 01-1 1h-2a1 1 0 01-1-1v-6z" />
              </svg>
              <div><p className="font-medium">ワークフロービルダー</p><p className="text-sm text-muted-foreground">解析パイプラインの構築</p></div>
            </Link>
          </div>
        </div>
      </div>

      {recentNotifications.length > 0 && (
        <div className="rounded-lg border bg-card">
          <div className="border-b px-6 py-4"><h2 className="font-semibold">最近のアクティビティ</h2></div>
          <div className="divide-y">
            {recentNotifications.map((notification) => (
              <div key={notification.id} className="flex items-center gap-4 px-6 py-3">
                <div className={`h-2 w-2 rounded-full ${notification.type === 'error' ? 'bg-red-500' : notification.type === 'warning' ? 'bg-yellow-500' : notification.type === 'success' ? 'bg-green-500' : 'bg-blue-500'}`} />
                <p className="flex-1 text-sm">{notification.message}</p>
                <time className="text-xs text-muted-foreground">{new Date(notification.timestamp).toLocaleTimeString()}</time>
              </div>
            ))}
          </div>
        </div>
      )}
    </div>
  )
}
