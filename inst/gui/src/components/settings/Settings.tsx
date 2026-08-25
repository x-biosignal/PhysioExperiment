import { useState } from 'react'
import { useAppStore } from '@/stores/appStore'

type SettingsTab = 'general' | 'appearance' | 'visualization' | 'api' | 'keyboard'

interface TabProps {
  label: string
  icon: React.ReactNode
  isActive: boolean
  onClick: () => void
}

function Tab({ label, icon, isActive, onClick }: TabProps) {
  return (
    <button
      onClick={onClick}
      className={`flex items-center gap-3 w-full px-4 py-2 text-sm text-left rounded-lg transition-colors ${
        isActive
          ? 'bg-primary/10 text-primary'
          : 'hover:bg-accent'
      }`}
    >
      {icon}
      {label}
    </button>
  )
}

function GeneralSettings() {
  const { language, setLanguage } = useAppStore()

  return (
    <div className="space-y-6">
      <div>
        <h3 className="text-lg font-medium">一般設定</h3>
        <p className="text-sm text-muted-foreground">
          アプリケーションの基本設定を変更します
        </p>
      </div>

      <div className="space-y-4">
        <div>
          <label className="block text-sm font-medium">言語</label>
          <select
            value={language}
            onChange={(e) => setLanguage(e.target.value as 'ja' | 'en')}
            className="mt-1 w-full max-w-xs rounded-md border bg-background px-3 py-2 text-sm"
          >
            <option value="ja">日本語</option>
            <option value="en">English</option>
          </select>
        </div>

        <div>
          <label className="block text-sm font-medium">自動保存</label>
          <div className="mt-2 space-y-2">
            <label className="flex items-center gap-2">
              <input type="checkbox" defaultChecked className="rounded" />
              <span className="text-sm">ワークフローを自動保存</span>
            </label>
            <label className="flex items-center gap-2">
              <input type="checkbox" defaultChecked className="rounded" />
              <span className="text-sm">設定を自動保存</span>
            </label>
          </div>
        </div>

        <div>
          <label className="block text-sm font-medium">起動時の動作</label>
          <select className="mt-1 w-full max-w-xs rounded-md border bg-background px-3 py-2 text-sm">
            <option>ダッシュボードを表示</option>
            <option>前回のセッションを復元</option>
            <option>データブラウザを表示</option>
          </select>
        </div>
      </div>
    </div>
  )
}

function AppearanceSettings() {
  const { theme, setTheme } = useAppStore()

  return (
    <div className="space-y-6">
      <div>
        <h3 className="text-lg font-medium">外観</h3>
        <p className="text-sm text-muted-foreground">
          テーマとカラースキームを設定します
        </p>
      </div>

      <div className="space-y-4">
        <div>
          <label className="block text-sm font-medium">テーマ</label>
          <div className="mt-2 grid grid-cols-3 gap-3 max-w-md">
            {(['light', 'dark', 'system'] as const).map((t) => (
              <button
                key={t}
                onClick={() => setTheme(t)}
                className={`rounded-lg border p-4 text-center transition-colors ${
                  theme === t
                    ? 'border-primary bg-primary/10'
                    : 'hover:bg-accent'
                }`}
              >
                <div className={`mx-auto h-8 w-8 rounded-full ${
                  t === 'light' ? 'bg-white border' :
                  t === 'dark' ? 'bg-gray-800' :
                  'bg-gradient-to-r from-white to-gray-800'
                }`} />
                <span className="mt-2 block text-sm">
                  {t === 'light' ? 'ライト' : t === 'dark' ? 'ダーク' : 'システム'}
                </span>
              </button>
            ))}
          </div>
        </div>

        <div>
          <label className="block text-sm font-medium">アクセントカラー</label>
          <div className="mt-2 flex gap-2">
            {['#3b82f6', '#22c55e', '#a855f7', '#f97316', '#ef4444'].map((color) => (
              <button
                key={color}
                className="h-8 w-8 rounded-full border-2 border-transparent hover:border-foreground/50"
                style={{ backgroundColor: color }}
              />
            ))}
          </div>
        </div>

        <div>
          <label className="block text-sm font-medium">フォントサイズ</label>
          <div className="mt-2 flex items-center gap-4">
            <input
              type="range"
              min="12"
              max="18"
              defaultValue="14"
              className="w-48"
            />
            <span className="text-sm">14px</span>
          </div>
        </div>

        <div>
          <label className="block text-sm font-medium">コンパクトモード</label>
          <div className="mt-2">
            <label className="flex items-center gap-2">
              <input type="checkbox" className="rounded" />
              <span className="text-sm">UIの密度を上げる</span>
            </label>
          </div>
        </div>
      </div>
    </div>
  )
}

function VisualizationSettings() {
  return (
    <div className="space-y-6">
      <div>
        <h3 className="text-lg font-medium">可視化設定</h3>
        <p className="text-sm text-muted-foreground">
          グラフと信号表示の設定を変更します
        </p>
      </div>

      <div className="space-y-4">
        <div>
          <label className="block text-sm font-medium">デフォルトカラーマップ</label>
          <select className="mt-1 w-full max-w-xs rounded-md border bg-background px-3 py-2 text-sm">
            <option>viridis</option>
            <option>plasma</option>
            <option>inferno</option>
            <option>magma</option>
            <option>jet</option>
            <option>coolwarm</option>
          </select>
        </div>

        <div>
          <label className="block text-sm font-medium">信号ビューア</label>
          <div className="mt-2 space-y-2">
            <div className="flex items-center justify-between max-w-xs">
              <span className="text-sm">デフォルト表示時間 (秒)</span>
              <input
                type="number"
                defaultValue={10}
                min={1}
                max={60}
                className="w-20 rounded-md border bg-background px-3 py-1 text-sm"
              />
            </div>
            <div className="flex items-center justify-between max-w-xs">
              <span className="text-sm">最大表示チャンネル数</span>
              <input
                type="number"
                defaultValue={16}
                min={1}
                max={64}
                className="w-20 rounded-md border bg-background px-3 py-1 text-sm"
              />
            </div>
          </div>
        </div>

        <div>
          <label className="block text-sm font-medium">スペクトログラム</label>
          <div className="mt-2 space-y-2">
            <div className="flex items-center justify-between max-w-xs">
              <span className="text-sm">デフォルトウィンドウサイズ</span>
              <select className="w-20 rounded-md border bg-background px-2 py-1 text-sm">
                <option>128</option>
                <option>256</option>
                <option>512</option>
                <option>1024</option>
              </select>
            </div>
            <div className="flex items-center justify-between max-w-xs">
              <span className="text-sm">デフォルトオーバーラップ</span>
              <select className="w-20 rounded-md border bg-background px-2 py-1 text-sm">
                <option>25%</option>
                <option>50%</option>
                <option>75%</option>
              </select>
            </div>
          </div>
        </div>

        <div>
          <label className="block text-sm font-medium">パフォーマンス</label>
          <div className="mt-2 space-y-2">
            <label className="flex items-center gap-2">
              <input type="checkbox" defaultChecked className="rounded" />
              <span className="text-sm">WebGL アクセラレーション</span>
            </label>
            <label className="flex items-center gap-2">
              <input type="checkbox" defaultChecked className="rounded" />
              <span className="text-sm">大規模データの自動ダウンサンプリング</span>
            </label>
          </div>
        </div>
      </div>
    </div>
  )
}

function ApiSettings() {
  return (
    <div className="space-y-6">
      <div>
        <h3 className="text-lg font-medium">API設定</h3>
        <p className="text-sm text-muted-foreground">
          バックエンドサーバーとの接続設定を変更します
        </p>
      </div>

      <div className="space-y-4">
        <div>
          <label className="block text-sm font-medium">APIサーバーURL</label>
          <input
            type="text"
            defaultValue="http://localhost:8000"
            className="mt-1 w-full max-w-md rounded-md border bg-background px-3 py-2 text-sm"
          />
        </div>

        <div>
          <label className="block text-sm font-medium">WebSocket URL</label>
          <input
            type="text"
            defaultValue="ws://localhost:8000/ws"
            className="mt-1 w-full max-w-md rounded-md border bg-background px-3 py-2 text-sm"
          />
        </div>

        <div>
          <label className="block text-sm font-medium">タイムアウト (秒)</label>
          <input
            type="number"
            defaultValue={30}
            min={5}
            max={300}
            className="mt-1 w-32 rounded-md border bg-background px-3 py-2 text-sm"
          />
        </div>

        <div>
          <label className="block text-sm font-medium">認証</label>
          <div className="mt-2 space-y-2">
            <label className="flex items-center gap-2">
              <input type="checkbox" className="rounded" />
              <span className="text-sm">API キー認証を有効化</span>
            </label>
            <input
              type="password"
              placeholder="API キー"
              className="w-full max-w-md rounded-md border bg-background px-3 py-2 text-sm"
              disabled
            />
          </div>
        </div>

        <div className="pt-4">
          <button className="rounded-md bg-primary px-4 py-2 text-sm text-primary-foreground hover:bg-primary/90">
            接続テスト
          </button>
        </div>
      </div>
    </div>
  )
}

function KeyboardSettings() {
  const shortcuts = [
    { key: 'Ctrl + O', action: 'ファイルを開く' },
    { key: 'Ctrl + S', action: '保存' },
    { key: 'Ctrl + Z', action: '元に戻す' },
    { key: 'Ctrl + Shift + Z', action: 'やり直す' },
    { key: 'Space', action: '再生/停止' },
    { key: '←/→', action: '時間移動' },
    { key: '+/-', action: 'ズームイン/アウト' },
    { key: 'Ctrl + 1-5', action: 'パネル切り替え' },
    { key: 'F11', action: 'フルスクリーン' },
    { key: 'Esc', action: '選択解除' },
  ]

  return (
    <div className="space-y-6">
      <div>
        <h3 className="text-lg font-medium">キーボードショートカット</h3>
        <p className="text-sm text-muted-foreground">
          キーボードショートカットの一覧と設定
        </p>
      </div>

      <div className="rounded-lg border">
        <table className="w-full">
          <thead>
            <tr className="border-b bg-muted/50">
              <th className="px-4 py-3 text-left text-sm font-medium">ショートカット</th>
              <th className="px-4 py-3 text-left text-sm font-medium">アクション</th>
            </tr>
          </thead>
          <tbody className="divide-y">
            {shortcuts.map((shortcut, i) => (
              <tr key={i} className="hover:bg-accent/50">
                <td className="px-4 py-3">
                  <kbd className="rounded bg-muted px-2 py-1 text-xs font-mono">
                    {shortcut.key}
                  </kbd>
                </td>
                <td className="px-4 py-3 text-sm">{shortcut.action}</td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>

      <div>
        <button className="rounded-md border px-4 py-2 text-sm hover:bg-accent">
          ショートカットをリセット
        </button>
      </div>
    </div>
  )
}

export function Settings() {
  const [activeTab, setActiveTab] = useState<SettingsTab>('general')

  const tabs: { id: SettingsTab; label: string; icon: React.ReactNode }[] = [
    {
      id: 'general',
      label: '一般',
      icon: (
        <svg className="h-4 w-4" fill="none" viewBox="0 0 24 24" stroke="currentColor">
          <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M10.325 4.317c.426-1.756 2.924-1.756 3.35 0a1.724 1.724 0 002.573 1.066c1.543-.94 3.31.826 2.37 2.37a1.724 1.724 0 001.065 2.572c1.756.426 1.756 2.924 0 3.35a1.724 1.724 0 00-1.066 2.573c.94 1.543-.826 3.31-2.37 2.37a1.724 1.724 0 00-2.572 1.065c-.426 1.756-2.924 1.756-3.35 0a1.724 1.724 0 00-2.573-1.066c-1.543.94-3.31-.826-2.37-2.37a1.724 1.724 0 00-1.065-2.572c-1.756-.426-1.756-2.924 0-3.35a1.724 1.724 0 001.066-2.573c-.94-1.543.826-3.31 2.37-2.37.996.608 2.296.07 2.572-1.065z" />
          <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M15 12a3 3 0 11-6 0 3 3 0 016 0z" />
        </svg>
      )
    },
    {
      id: 'appearance',
      label: '外観',
      icon: (
        <svg className="h-4 w-4" fill="none" viewBox="0 0 24 24" stroke="currentColor">
          <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M7 21a4 4 0 01-4-4V5a2 2 0 012-2h4a2 2 0 012 2v12a4 4 0 01-4 4zm0 0h12a2 2 0 002-2v-4a2 2 0 00-2-2h-2.343M11 7.343l1.657-1.657a2 2 0 012.828 0l2.829 2.829a2 2 0 010 2.828l-8.486 8.485M7 17h.01" />
        </svg>
      )
    },
    {
      id: 'visualization',
      label: '可視化',
      icon: (
        <svg className="h-4 w-4" fill="none" viewBox="0 0 24 24" stroke="currentColor">
          <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M9 19v-6a2 2 0 00-2-2H5a2 2 0 00-2 2v6a2 2 0 002 2h2a2 2 0 002-2zm0 0V9a2 2 0 012-2h2a2 2 0 012 2v10m-6 0a2 2 0 002 2h2a2 2 0 002-2m0 0V5a2 2 0 012-2h2a2 2 0 012 2v14a2 2 0 01-2 2h-2a2 2 0 01-2-2z" />
        </svg>
      )
    },
    {
      id: 'api',
      label: 'API',
      icon: (
        <svg className="h-4 w-4" fill="none" viewBox="0 0 24 24" stroke="currentColor">
          <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M5 12h14M5 12a2 2 0 01-2-2V6a2 2 0 012-2h14a2 2 0 012 2v4a2 2 0 01-2 2M5 12a2 2 0 00-2 2v4a2 2 0 002 2h14a2 2 0 002-2v-4a2 2 0 00-2-2m-2-4h.01M17 16h.01" />
        </svg>
      )
    },
    {
      id: 'keyboard',
      label: 'ショートカット',
      icon: (
        <svg className="h-4 w-4" fill="none" viewBox="0 0 24 24" stroke="currentColor">
          <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M12 3v1m0 16v1m9-9h-1M4 12H3m15.364 6.364l-.707-.707M6.343 6.343l-.707-.707m12.728 0l-.707.707M6.343 17.657l-.707.707M16 12a4 4 0 11-8 0 4 4 0 018 0z" />
        </svg>
      )
    },
  ]

  return (
    <div className="flex h-full">
      {/* Sidebar */}
      <div className="w-56 flex-shrink-0 border-r p-4">
        <h1 className="mb-4 text-lg font-semibold">設定</h1>
        <nav className="space-y-1">
          {tabs.map((tab) => (
            <Tab
              key={tab.id}
              label={tab.label}
              icon={tab.icon}
              isActive={activeTab === tab.id}
              onClick={() => setActiveTab(tab.id)}
            />
          ))}
        </nav>
      </div>

      {/* Content */}
      <div className="flex-1 overflow-auto p-8">
        <div className="mx-auto max-w-2xl">
          {activeTab === 'general' && <GeneralSettings />}
          {activeTab === 'appearance' && <AppearanceSettings />}
          {activeTab === 'visualization' && <VisualizationSettings />}
          {activeTab === 'api' && <ApiSettings />}
          {activeTab === 'keyboard' && <KeyboardSettings />}
        </div>
      </div>
    </div>
  )
}
