import { useState } from 'react'
import { useNavigate } from 'react-router-dom'
import { useDataStore, type Dataset } from '@/stores/dataStore'
import { formatDuration } from '@/lib/utils'

interface ImportDialogProps {
  isOpen: boolean
  onClose: () => void
  onImport: (path: string, format?: string) => void
}

function ImportDialog({ isOpen, onClose, onImport }: ImportDialogProps) {
  const [path, setPath] = useState('')
  const [format, setFormat] = useState<string>('auto')

  if (!isOpen) return null

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault()
    onImport(path, format === 'auto' ? undefined : format)
    setPath('')
    setFormat('auto')
    onClose()
  }

  return (
    <div className="fixed inset-0 z-50 flex items-center justify-center bg-black/50">
      <div className="w-full max-w-md rounded-lg bg-card p-6 shadow-lg">
        <h2 className="text-lg font-semibold">データファイルをインポート</h2>
        <form onSubmit={handleSubmit} className="mt-4 space-y-4">
          <div>
            <label className="block text-sm font-medium">ファイルパス</label>
            <input
              type="text"
              value={path}
              onChange={(e) => setPath(e.target.value)}
              placeholder="/path/to/data.edf"
              className="mt-1 w-full rounded-md border bg-background px-3 py-2 text-sm focus:border-primary focus:outline-none focus:ring-1 focus:ring-primary"
              required
            />
          </div>
          <div>
            <label className="block text-sm font-medium">ファイル形式</label>
            <select
              value={format}
              onChange={(e) => setFormat(e.target.value)}
              className="mt-1 w-full rounded-md border bg-background px-3 py-2 text-sm focus:border-primary focus:outline-none focus:ring-1 focus:ring-primary"
            >
              <option value="auto">自動検出</option>
              <option value="edf">EDF / EDF+</option>
              <option value="bdf">BDF / BDF+</option>
              <option value="brainvision">BrainVision</option>
              <option value="hdf5">HDF5</option>
              <option value="csv">CSV</option>
            </select>
          </div>
          <div className="flex justify-end gap-2">
            <button type="button" onClick={onClose} className="rounded-md px-4 py-2 text-sm hover:bg-accent">キャンセル</button>
            <button type="submit" className="rounded-md bg-primary px-4 py-2 text-sm text-primary-foreground hover:bg-primary/90">インポート</button>
          </div>
        </form>
      </div>
    </div>
  )
}

interface DatasetCardProps {
  dataset: Dataset
  isActive: boolean
  onSelect: () => void
  onDelete: () => void
}

function DatasetCard({ dataset, isActive, onSelect, onDelete }: DatasetCardProps) {
  const isDemo = dataset.metadata?.isDemo as boolean
  const dataType = dataset.metadata?.dataType as string

  const getTypeColor = (type: string) => {
    switch (type) {
      case 'EEG': return 'bg-blue-500/10 text-blue-600 border-blue-500/30'
      case 'ECG': return 'bg-red-500/10 text-red-600 border-red-500/30'
      case 'EMG': return 'bg-green-500/10 text-green-600 border-green-500/30'
      case 'MEG': return 'bg-purple-500/10 text-purple-600 border-purple-500/30'
      default: return 'bg-gray-500/10 text-gray-600 border-gray-500/30'
    }
  }

  return (
    <div
      className={`rounded-lg border p-4 transition-colors cursor-pointer ${isActive ? 'border-primary bg-primary/5' : 'hover:bg-accent'}`}
      onClick={onSelect}
    >
      <div className="flex items-start justify-between">
        <div className="flex-1 min-w-0">
          <div className="flex items-center gap-2">
            <h3 className="font-medium truncate">{dataset.name}</h3>
            {dataType && (
              <span className={`rounded border px-1.5 py-0.5 text-xs font-medium ${getTypeColor(dataType)}`}>
                {dataType}
              </span>
            )}
            {isDemo && (
              <span className="rounded bg-amber-500/10 border border-amber-500/30 px-1.5 py-0.5 text-xs font-medium text-amber-600">
                デモ
              </span>
            )}
          </div>
          <p className="text-sm text-muted-foreground mt-1">ID: {dataset.id}</p>
        </div>
        <button
          onClick={(e) => { e.stopPropagation(); onDelete() }}
          className="rounded p-1 text-muted-foreground hover:bg-destructive/10 hover:text-destructive"
          title="削除"
        >
          <svg className="h-4 w-4" fill="none" viewBox="0 0 24 24" stroke="currentColor">
            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M19 7l-.867 12.142A2 2 0 0116.138 21H7.862a2 2 0 01-1.995-1.858L5 7m5 4v6m4-6v6m1-10V4a1 1 0 00-1-1h-4a1 1 0 00-1 1v3M4 7h16" />
          </svg>
        </button>
      </div>

      <div className="mt-4 grid grid-cols-2 gap-2 text-sm">
        <div><span className="text-muted-foreground">チャンネル数:</span><span className="ml-1 font-medium">{dataset.nChannels}</span></div>
        <div><span className="text-muted-foreground">サンプリング:</span><span className="ml-1 font-medium">{dataset.samplingRate} Hz</span></div>
        <div><span className="text-muted-foreground">時間長:</span><span className="ml-1 font-medium">{formatDuration(dataset.duration)}</span></div>
        <div><span className="text-muted-foreground">時点数:</span><span className="ml-1 font-medium">{dataset.nTimepoints.toLocaleString()}</span></div>
      </div>

      {dataset.assays && dataset.assays.length > 0 && (
        <div className="mt-3">
          <span className="text-xs text-muted-foreground">アッセイ:</span>
          <div className="mt-1 flex flex-wrap gap-1">
            {dataset.assays.map((assay) => (
              <span key={assay} className="rounded bg-secondary px-2 py-0.5 text-xs">{assay}</span>
            ))}
          </div>
        </div>
      )}

      {dataset.channels && dataset.channels.length > 0 && (
        <div className="mt-3">
          <span className="text-xs text-muted-foreground">
            チャンネル: {dataset.channels.slice(0, 5).map(ch => ch.label).join(', ')}
            {dataset.channels.length > 5 && ` +${dataset.channels.length - 5}`}
          </span>
        </div>
      )}
    </div>
  )
}

interface DatasetDetailProps {
  dataset: Dataset | null
  onNavigate: (page: string, datasetId: string) => void
}

function DatasetDetail({ dataset, onNavigate }: DatasetDetailProps) {
  if (!dataset) {
    return (
      <div className="flex h-full items-center justify-center text-muted-foreground">
        <div className="text-center">
          <svg className="mx-auto h-12 w-12" fill="none" viewBox="0 0 24 24" stroke="currentColor">
            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={1.5} d="M9 5H7a2 2 0 00-2 2v12a2 2 0 002 2h10a2 2 0 002-2V7a2 2 0 00-2-2h-2M9 5a2 2 0 002 2h2a2 2 0 002-2M9 5a2 2 0 012-2h2a2 2 0 012 2" />
          </svg>
          <p className="mt-4">データセットを選択してください</p>
        </div>
      </div>
    )
  }

  const channelLabels = dataset.channels.map(ch => ch.label)

  return (
    <div className="h-full overflow-auto p-6">
      <h2 className="text-xl font-semibold">{dataset.name}</h2>

      <section className="mt-6">
        <h3 className="text-sm font-medium text-muted-foreground">基本情報</h3>
        <dl className="mt-2 grid grid-cols-2 gap-4">
          <div><dt className="text-sm text-muted-foreground">ID</dt><dd className="font-mono text-sm">{dataset.id}</dd></div>
          <div><dt className="text-sm text-muted-foreground">サンプリングレート</dt><dd className="font-medium">{dataset.samplingRate} Hz</dd></div>
          <div><dt className="text-sm text-muted-foreground">記録時間</dt><dd className="font-medium">{formatDuration(dataset.duration)}</dd></div>
          <div><dt className="text-sm text-muted-foreground">時点数</dt><dd className="font-medium">{dataset.nTimepoints.toLocaleString()}</dd></div>
        </dl>
      </section>

      <section className="mt-6">
        <h3 className="text-sm font-medium text-muted-foreground">チャンネル ({dataset.nChannels})</h3>
        <div className="mt-2 max-h-40 overflow-auto rounded-md border bg-secondary/30 p-3">
          <div className="flex flex-wrap gap-1">
            {channelLabels.map((ch, i) => (
              <span key={i} className="rounded bg-background px-2 py-1 text-xs font-mono">{ch}</span>
            ))}
          </div>
        </div>
      </section>

      <section className="mt-6">
        <h3 className="text-sm font-medium text-muted-foreground">アッセイ ({dataset.assays?.length || 0})</h3>
        <div className="mt-2 space-y-2">
          {dataset.assays?.map((assay) => (
            <div key={assay} className="flex items-center justify-between rounded-md border p-3">
              <span className="font-medium">{assay}</span>
            </div>
          )) || <p className="text-sm text-muted-foreground">アッセイなし</p>}
        </div>
      </section>

      {dataset.events && dataset.events.length > 0 && (
        <section className="mt-6">
          <h3 className="text-sm font-medium text-muted-foreground">イベント ({dataset.events.length})</h3>
          <div className="mt-2 max-h-40 overflow-auto">
            <table className="w-full text-sm">
              <thead className="sticky top-0 bg-card">
                <tr className="border-b">
                  <th className="py-2 text-left font-medium">タイプ</th>
                  <th className="py-2 text-left font-medium">開始</th>
                  <th className="py-2 text-left font-medium">期間</th>
                </tr>
              </thead>
              <tbody className="divide-y">
                {dataset.events.slice(0, 20).map((event, i) => (
                  <tr key={i}>
                    <td className="py-2">{event.type}</td>
                    <td className="py-2">{event.onset.toFixed(3)}s</td>
                    <td className="py-2">{event.duration.toFixed(3)}s</td>
                  </tr>
                ))}
              </tbody>
            </table>
            {dataset.events.length > 20 && <p className="mt-2 text-xs text-muted-foreground">...他 {dataset.events.length - 20} イベント</p>}
          </div>
        </section>
      )}

      {dataset.metadata && Object.keys(dataset.metadata).length > 0 && (
        <section className="mt-6">
          <h3 className="text-sm font-medium text-muted-foreground">メタデータ</h3>
          <pre className="mt-2 max-h-40 overflow-auto rounded-md bg-secondary/30 p-3 text-xs">
            {JSON.stringify(dataset.metadata, null, 2)}
          </pre>
        </section>
      )}

      <section className="mt-6 flex gap-2">
        <button
          onClick={() => onNavigate('/visualization', dataset.id)}
          className="rounded-md bg-primary px-4 py-2 text-sm text-primary-foreground hover:bg-primary/90"
        >
          ビューアで開く
        </button>
        <button
          onClick={() => onNavigate('/analysis', dataset.id)}
          className="rounded-md border px-4 py-2 text-sm hover:bg-accent"
        >
          解析を開始
        </button>
      </section>
    </div>
  )
}

export function DataBrowser() {
  const { datasets, activeDatasetId, setActiveDataset, removeDataset, loadTestDatasets } = useDataStore()
  const navigate = useNavigate()
  const [showImport, setShowImport] = useState(false)
  const [searchQuery, setSearchQuery] = useState('')
  const [isLoadingTest, setIsLoadingTest] = useState(false)

  const handleLoadTestData = () => {
    setIsLoadingTest(true)
    // Simulate a small delay to show loading state
    setTimeout(() => {
      loadTestDatasets()
      // Auto-select first dataset after loading
      const store = useDataStore.getState()
      const datasetIds = Object.keys(store.datasets)
      if (datasetIds.length > 0 && !store.activeDatasetId) {
        setActiveDataset(datasetIds[0])
      }
      setIsLoadingTest(false)
    }, 500)
  }

  const handleNavigate = (page: string, datasetId: string) => {
    console.log('Navigating to:', page, 'with dataset:', datasetId)
    // Ensure dataset is set before navigation
    setActiveDataset(datasetId)
    // Small delay to ensure state is updated
    setTimeout(() => {
      navigate(page)
    }, 10)
  }

  const datasetList = Object.values(datasets)
  const filteredDatasets = datasetList.filter((d) =>
    d.name.toLowerCase().includes(searchQuery.toLowerCase()) ||
    d.id.toLowerCase().includes(searchQuery.toLowerCase())
  )

  const activeDataset = activeDatasetId ? datasets[activeDatasetId] : null

  const handleImport = async (path: string, format?: string) => {
    console.log('Import:', path, format)
  }

  return (
    <div className="flex h-full">
      <div className="w-80 flex-shrink-0 border-r flex flex-col">
        <div className="border-b p-4">
          <div className="flex items-center justify-between">
            <h2 className="font-semibold">データセット</h2>
            <button onClick={() => setShowImport(true)} className="rounded-md bg-primary p-2 text-primary-foreground hover:bg-primary/90" title="インポート">
              <svg className="h-4 w-4" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M12 4v16m8-8H4" />
              </svg>
            </button>
          </div>
          <input
            type="text"
            placeholder="検索..."
            value={searchQuery}
            onChange={(e) => setSearchQuery(e.target.value)}
            className="mt-3 w-full rounded-md border bg-background px-3 py-2 text-sm focus:border-primary focus:outline-none focus:ring-1 focus:ring-primary"
          />
        </div>
        <div className="flex-1 overflow-auto p-4">
          {filteredDatasets.length === 0 ? (
            <div className="py-8 text-center text-muted-foreground">
              {datasetList.length === 0 ? (
                <>
                  <svg className="mx-auto h-12 w-12 text-muted-foreground/50" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                    <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={1.5} d="M4 7v10c0 2.21 3.582 4 8 4s8-1.79 8-4V7M4 7c0 2.21 3.582 4 8 4s8-1.79 8-4M4 7c0-2.21 3.582-4 8-4s8 1.79 8 4m0 5c0 2.21-3.582 4-8 4s-8-1.79-8-4" />
                  </svg>
                  <p className="mt-4">データセットがありません</p>
                  <div className="mt-4 flex flex-col gap-2 items-center">
                    <button
                      onClick={handleLoadTestData}
                      disabled={isLoadingTest}
                      className="rounded-md bg-primary px-4 py-2 text-sm text-primary-foreground hover:bg-primary/90 disabled:opacity-50"
                    >
                      {isLoadingTest ? '読み込み中...' : 'デモデータを読み込み'}
                    </button>
                    <button onClick={() => setShowImport(true)} className="text-sm text-primary hover:underline">
                      ファイルをインポート
                    </button>
                  </div>
                </>
              ) : <p>検索結果なし</p>}
            </div>
          ) : (
            <div className="space-y-3">
              {filteredDatasets.map((dataset) => (
                <DatasetCard
                  key={dataset.id}
                  dataset={dataset}
                  isActive={dataset.id === activeDatasetId}
                  onSelect={() => setActiveDataset(dataset.id)}
                  onDelete={() => removeDataset(dataset.id)}
                />
              ))}
            </div>
          )}
        </div>
      </div>

      <div className="flex-1">
        <DatasetDetail dataset={activeDataset} onNavigate={handleNavigate} />
      </div>

      <ImportDialog isOpen={showImport} onClose={() => setShowImport(false)} onImport={handleImport} />
    </div>
  )
}
